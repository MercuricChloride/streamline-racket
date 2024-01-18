#lang racket/base

(require racket/string
         racket/port
         racket/system
         racket/list
         racket/bool
         racket/match
         racket/pretty
         dali
         yaml
         "./lexer.rkt"
         "./parser.rkt"
         "./yaml.rkt"
         "./macros.rkt"
         "./utils.rkt"
         "./module-utils.rkt")

; The dali template library escapes a bunch of characters, we don't need this.
; So we just set the escape-replacements parameter to be effectively nothing. (it checks for an empty list)
(escape-replacements '(("&" . "&")))

;; ==========================================
;; PARAMETERS
;; ==========================================
(define local-var-names (make-parameter '()))
(define store-type (make-parameter "StoreSetProto<prost_wkt_types::Struct>"))
;; The current context allows us to generate our code differently based on what scope we are in. IE assignments, callback etc.
(define current-context (make-parameter 'global))

;; ==========================================
;; CONSTANTS
;; ==========================================
(define streamline-path
  (string-replace (with-output-to-string (lambda () (system "echo $HOME/.streamline/"))) "\n" ""))

;; ==========================================
;; MACROS
;; ==========================================
(define-syntax-rule (w-attributes attributes expression)
  (parameterize ([local-var-names (get-local-vars attributes)]
                 [store-type (get-store-type attributes)])
    expression))

(define-syntax-rule (w-context context expression)
  (parameterize ([current-context context])
    expression))

;; Recursively generate code
(define-syntax-rule (gen proc arg ...) (proc (generate-code arg) ...))

;; ==========================================
;; STRUCTS
;; ==========================================

;; Data for generating contract source code
(struct source-data (sol-macro name events))

;; ==========================================
;; Attribute Helpers
;; ==========================================

(define (attr-vars/map attributes proc)
  (filter-map (match-lambda
                [(kv-attribute "var" name value) (proc name value)]
                [_ false])
              attributes))

(define (attr-vars/gen)
  (w-sep "\n"
         (map (match-lambda
                [(list name value)
                 (format "let mut ~a:std::rc::Rc<LocalVar> = std::rc::Rc::new(LocalVar::from(~a));"
                         name
                         (generate-code value))])
              (local-var-names))))

(define (get-local-vars attributes)
  (filter-map (match-lambda
                [(kv-attribute "var" name value) (list name value)]
                [_ false])
              attributes))

;; Returns true if the value of VAR is a member of the local variables in scope
(define (local-var? var)
  (define vars (map first (local-var-names)))
  (member var vars))

;; ==========================================
;; TEMPLATES
;; ==========================================

(def-template
 (get-events/gen event-name abi address)
 (address (string-downcase (substring address 2)))
 "
    \"{{event-name}}\"; {{abi}}::{{event-name}}::get_events(&blk, &[&address!(\"{{address}}\")])")

(define (source-def/gen path)
  ;; Grab the source code
  (define source-code (read-file path))

  ;; Grab the name of the source
  (define source-name
    (as~> v
          (regexp-match* #rx"(?:interface|contract) ([a-zA-Z$_][a-zA-Z0-9$_]*)"
                         source-code
                         #:match-select cadr)
          (first v)))
  ;; Grab the events from the source
  (define events
    ;; NOTE I think I can replace this with identity
    (as~>
     v
     (filter (lambda (i) (not (false? i)))
             (regexp-match* #rx"event ([a-zA-Z$_][a-zA-Z0-9$_]*)\\(" source-code #:match-select cadr))
     (flatten v)))
  ;; Generate the sol-macro implementation
  (define sol-macro (derive-sol-traits source-code))

  ;; Construct the source-data struct
  (source-data sol-macro source-name events))

(define (module-inputs/gen inputs)
  (w-sep ","
         (minput-map
          inputs
          (match-lambda
            [(sfn-delta-edge from) (format "~a: Deltas<DeltaProto<prost_wkt_types::Struct>>" from)]
            [?
             string?
             (format "~a: prost_wkt_types::Struct" ?)]))))

(def-template (format-inputs inputs)
              (input-names (w-sep "," (minput-map inputs minput->name)))
              "format_inputs!({{input-names}});")

(def-template
 (mfn/gen name inputs body)
 (module-inputs (module-inputs/gen inputs)
                formatted-inputs
                (format-inputs inputs)
                initial-value
                (output-map-init inputs)
                local-vars
                (attr-vars/gen))
 "
  #[substreams::handlers::map]
  fn {{name}}({{module-inputs}}) -> Option<prost_wkt_types::Struct> {
      {{local-vars}}
      {{formatted-inputs}}
      with_map! {output_map,
        {{initial-value}}
        {{body}}
    }
  }")

(def-template
 (sfn/gen name inputs body)
 (module-inputs (module-inputs/gen inputs)
                formatted-inputs
                (format-inputs inputs)
                initial-value
                (output-map-init inputs)
                local-vars
                (attr-vars/gen)
                store-kind
                (store-type))
 "
#[substreams::handlers::store]
fn {{name}}({{module-inputs}}, substreams_store_param: {{store-kind}}) {
    {{local-vars}}
    {{formatted-inputs}}
    {{initial-value}}
    {{body}}
}
")

(def-template (fn-input input) "{{input}}: &SolidityType")

(def-template
 (fn/gen name inputs body)
 (module-inputs (w-sep "," (map fn-input inputs))
                formatted-inputs
                (format-inputs inputs)
                initial-value
                (output-map-init inputs)
                local-vars
                (attr-vars/gen))
 "
fn {{name}}({{module-inputs}}) -> SolidityType {
    {{local-vars}}
    {{formatted-inputs}}
    {{initial-value}}
    {{body}}
    output_map
}
")

(define (fmt-args args)
  (string-join (map (lambda (_) "SolidityType") args) ","))

(def-template
 (lam/gen fn-args exprs)
 (args (w-sep "," fn-args) arg-types (fmt-args fn-args))
 "let output_map: SolidityType = (|({{args}}): ({{arg-types}})| -> SolidityType { {{exprs}} })(output_map);")

(def-template
 (hof/gen kind fn-args exprs)
 (args (w-sep "," fn-args) types (w-sep "," (map (λ (_) "SolidityType") fn-args)))
 "
  let output_map:SolidityType = {{kind}}!(output_map, |({{args}})| -> SolidityType { {{exprs}} });
")

(def-template (map-literal/gen kvs) (kvs (w-sep "," kvs)) "
map_literal!{
  {{kvs}}
}
")

(def-template (key-value/gen key val) " \"{{key}}\"; {{val}}")
(def-template (binary-op/gen lh op rh) "SolidityType::from(({{lh}} {{op}} {{rh}}))")
(def-template (field-access/gen lh rhs)
              (rhs (map (match-lambda
                          [(number-literal val) (format "\"~a\"" val)]
                          [? (format "\"~a\"" ?)])
                        rhs)
                   rhs
                   (w-sep "," rhs))
              "map_access!(&{{lh}}, {{rhs}})")

(def-template (rpc-call/gen name fn args) (args (w-sep "," args)) "{{name}}!({{fn}}Call, {{args}})")
(def-template (store-set/gen key value)
              "{substreams_store_param.generic_set({{key}}, {{value}}); SolidityType::Null}")
(def-template (store-delete/gen prefix)
              "{substreams_store_param.generic_delete_prefix({{prefix}}); SolidityType::Null}")
(def-template (store-get/gen ident key) "{{ident}}.generic_get({{key}})")
(def-template (do-block/gen exprs) (exprs (w-sep ";" exprs)) "{ {{exprs}} }")
(def-template (function-call/gen name args)
              (args (w-sep "," (map (λ (arg) (template "{{arg}}.into()" arg)) args)))
              "{{name}}({{args}})")

(def-template
 (var-assignment/gen var value)
 "{let mut mut_val = std::rc::Rc::get_mut({{var}}); mut_val = Some(&mut LocalVar::from({{value}})); SolidityType::Null}")
(def-template (sol-type type value) "sol_type!({{type}}, \"{{value}}\")")
(def-template (tuple-lit vals) (vals (w-sep "," vals)) "SolidityType::Tuple(vec![{{vals}}])")
(def-template (list-lit vals) (vals (w-sep "," vals)) "SolidityType::List(vec![{{vals}}])")

(define (instance-macro instance)
  (match-let ([(instance-def name abi address) instance])
    (template
     "
macro_rules! {{name}} {
    ($function: ident,  $($arg: expr),*) => { {
        let func_call = {{abi}}::$function::from((
            $($arg.into(),)*
        ));

        let calls = vec![RpcCall{
            to_addr: address!(\"
{{address}}
       \").to_vec(),
            data: func_call.abi_encode(),
        }];
        let rpc_calls = RpcCalls{ calls };
        let responses = substreams_ethereum::rpc::eth_call(&rpc_calls).responses;
        let responses = responses.into_iter()
                                 .map(|response| {{abi}}::$function::abi_decode_returns(&response.raw, false).expect(\"
       Couldn
       't
       decode
       return
       value
       of
       rpc
       call
       \"))
                                 .map(|return_value| serde_json::to_value(return_value).unwrap())
                                 .map(|value| SolidityType::guess_json_value(&value).unwrap())
                                 .collect::<Vec<SolidityType>>();
        responses.get(0).unwrap().clone()
    } };
}
"
     name
     abi
     address)))

;; ==========================================
;; CODE GENERATION FUNCTION
;; ==========================================
(define (generate-code node)
  (match node
    [(source-def path) (gen source-def/gen path)]
    [(rpc-call instance fn args) (gen rpc-call/gen instance fn args)]
    [(do-block exprs) (gen do-block/gen exprs)]
    [(map-literal kvs) (gen map-literal/gen kvs)]
    [(function-call name args) (gen function-call/gen name args)]
    [(key-value key val) (gen key-value/gen key val)]
    [(identifier name) (gen name)]
    [(binary-op lh op rh) (gen binary-op/gen lh op rh)]
    [(string-literal value) (sol-type "String" value)]
    [(number-literal value) (sol-type "Uint" value)]
    [(boolean-literal value) (sol-type "Boolean" (if value "1" "0"))]
    [(address-literal value) (sol-type "Address" value)]
    [(tuple-literal vals) (tuple-lit vals)]
    [(list-literal vals) (list-lit vals)]
    [(mfn name inputs body attributes) (w-attributes attributes (gen mfn/gen name inputs body))]
    [(sfn name inputs body attributes) (w-attributes attributes (gen sfn/gen name inputs body))]
    [(fn name inputs body attributes) (w-attributes attributes (gen fn/gen name inputs body))]
    [(var-assignment var value) (w-context 'assignment (gen var-assignment/gen var value))]
    [(store-set key value) (gen store-set/gen key value)]
    [(store-get ident key) (gen store-get/gen ident key)]
    [(store-delete prefix) (gen store-delete/gen prefix)]
    [(lam fn-args exprs) (gen lam/gen fn-args exprs)]
    [(hof kind (lam args body)) (w-context 'hof (gen hof/gen kind args body))]
    ;; TODO I should probably just remove this type
    [(pipeline functors) (w-sep "\n" (map generate-code functors))]
    [(field-access lh rhs) (gen field-access/gen lh rhs)]
    [(? string?)
     (if (local-var? node)
         (match (current-context)
           ['global node]
           ['hof (format "&~a" node)]
           ['assignment (format "&mut ~a" node)])
         node)]
    [(? number?) node]
    [(list item ...) (map generate-code node)]
    [_ node]))

(define (generate-yaml name modules)
  (define yaml-path (string-append streamline-path "substreams.yaml"))
  (write-string-to-file (string-append (yaml-string name) (yaml->string (hash "modules" modules)))
                        yaml-path))

(define (generate-streamline-file path)
  (define streamline-name (string-replace (last (string-split path "/")) ".strm" ""))
  (pretty-display (string-append "Generating Streamline File: " streamline-name))
  ; open a port to the source
  (define source-port (open-input-file path))
  (println "Opened source port")

  ; lex the file
  (define tokenized-input (tokenize source-port))
  (println "Tokenized Input")

  ; parse the file
  (define parsed-result (parse-file! tokenized-input))
  (define nodes (hash-ref parsed-result "nodes"))
  (generate-yaml streamline-name nodes)
  (define parsed-input (hash-ref parsed-result "parsed-file"))
  (println "Parsed Input")

  ; gather the instances
  (define contract-instances
    (filter (lambda (node)
              (match node
                [(instance-def _ _ _) true]
                [_ false]))
            parsed-input))
  (println "Got Contract Instances")

  ; gather the source defs
  (define source-defs
    (filter-map
     (lambda (node)
       (match node
         [(source-def path)
          (match-let ([(source-data sol-macro name events) (generate-code node)])
            (cons sol-macro (cons name events)))] ; we are returning this so we can construct a hash table from name and events with the rest of the returned list
         [_ false]))
     parsed-input))

  ; Creates a hash map from an abi instance -> all of the event names
  (define source-hash (make-hash (map rest source-defs)))

  (define instance-macros (string-join (map instance-macro contract-instances) "\n"))

  (define (instance-events instance)
    (match-let ([(instance-def name abi address) instance])
      (let* ([events (hash-ref source-hash abi)]
             [event-getters (map (lambda (event-name) (get-events/gen event-name abi address))
                                 events)])
        (list name (string-join event-getters ",")))))

  ; generate the code for the instances
  (define instances-def-code
    (format
     "
#[substreams::handlers::map]
fn EVENTS(blk: eth::Block) -> Option<prost_wkt_types::Struct> {
  with_map!{output_map,
  ~a
  }
}
"
     (string-join
      (let ([instance-events (map instance-events contract-instances)])
        (map
         (match-lambda
           [(list name events)
            (template
             "
        output_map.insert(\"{{name}}\", map_literal! {
{{instance-events}}
                    });"
             name
             events)])
         instance-events)))))

  ; generate  the code for the source defs
  (define source-def-code (string-join (map first source-defs)))

  ; generate the code for the rest of the nodes
  (define generated-code
    (let ([module-code (string-join (map generate-code
                                         (filter (lambda (node)
                                                   (match node
                                                     [(source-def _) false]
                                                     [(instance-def _ _ _) false]
                                                     [_ true]))
                                                 parsed-input)))])
      (string-append instance-macros source-def-code instances-def-code module-code)))
  (println "Generated Code")
  (write-string-to-file generated-code (string-append streamline-path "src/streamline.rs"))
  (println "Wrote output code")

  (pretty-display (with-output-to-string (lambda ()
                                           (system (format "cd ~a && make build" streamline-path)))))
  (println "Compiled Rust Code"))

(provide generate-streamline-file
         streamline-path)

(generate-streamline-file "examples/simpleErc721.strm")
