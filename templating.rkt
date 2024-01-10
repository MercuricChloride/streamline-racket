#lang racket

(require dali)
(require yaml)
(require "./lexer.rkt")
(require "./parser.rkt")

; The dali template library escapes a bunch of characters, we don't need this.
; So we just set the escape-replacements parameter to be effectively nothing. (it checks for an empty list)
(escape-replacements '(("&" . "&")))

(define streamline-path
  (string-replace (with-output-to-string (lambda () (system "echo $HOME/.streamline/"))) "\n" ""))

(define (yaml-string name)
  (expand-string
   "
specVersion: v0.1.0
package:
  name: {{name}}
  version: v0.1.0

imports:
  sql: https://github.com/streamingfast/substreams-sink-sql/releases/download/protodefs-v1.0.2/substreams-sink-sql-protodefs-v1.0.2.spkg
  database_change: https://github.com/streamingfast/substreams-sink-database-changes/releases/download/v1.2.1/substreams-database-change-v1.2.1.spkg

protobuf:
  files:
   - struct.proto
  importPaths:
    - ./proto

network: mainnet

binaries:
  default:
    type: wasm/rust-v1
    file: ./target/wasm32-unknown-unknown/release/streamline.wasm
"
   (hash "name" name)))

(define (expand str kvs)
  (expand-string str kvs))

(define-syntax-rule (gen proc args ...) (apply proc (map generate-code (list args ...))))

;; Data for generating contract source code
(struct source-data (sol-macro name events))

(define-syntax-rule (source path)
  (let* ([source-code (port->string (open-input-file path))]
         [events (regexp-match* #rx"event ([a-zA-Z$_][a-zA-Z0-9$_]*)\\([a-zA-Z$_][a-zA-Z0-9$_]*\\)"
                                source-code
                                #:match-select cadr)]
         [name (regexp-match* #rx"(?:interface|contract) ([a-zA-Z$_][a-zA-Z0-9$_]*)"
                              source-code
                              #:match-select cadr)]
         [sol-macro (expand "
loose_sol! {
  {{solidity}}
}
" (hash "solidity" source-code))])
    (contract-instance sol-macro name events)))

(define (get-events/gen event-name abi address)
  (expand
   "
    \"{{event-name}}\"; {{abi}}::{{event-name}}::get_events(&blk, &[&address!(\"{{address}}\")])
"
   (hash "event-name" event-name "abi" abi "address" (string-downcase (substring address 2)))))

(define (sources parser-result)
  (filter (lambda (item) (eq? (first item) 'source)) parser-result))

(define (instances parser-result)
  (filter (lambda (item) (eq? (first item) 'instance)) parser-result))

(define (source-def/gen path)
  (let* ([source-code (port->string (open-input-file path))]
         [events (filter (lambda (i) (not (false? i)))
                         (regexp-match* #rx"event ([a-zA-Z$_][a-zA-Z0-9$_]*)\\("
                                        source-code
                                        #:match-select cadr))]
         [name (regexp-match* #rx"(?:interface|contract) ([a-zA-Z$_][a-zA-Z0-9$_]*)"
                              source-code
                              #:match-select cadr)]
         [replaced-source (string-replace source-code
                                          "contract"
                                          "#[derive(Serialize, Deserialize, Debug)]\ncontract"
                                          #:all? true)]
         [replaced-source (string-replace replaced-source
                                          "interface"
                                          "#[derive(Serialize, Deserialize, Debug)]\ninterface"
                                          #:all? true)]
         [replaced-source (string-replace replaced-source
                                          "struct"
                                          "#[derive(Serialize, Deserialize, Debug)]\nstruct"
                                          #:all? true)]
         [replaced-source (string-replace replaced-source
                                          "enum"
                                          "#[derive(Serialize, Deserialize, Debug)]\nenum"
                                          #:all? true)]
         [sol-macro (expand "
sol! {
  {{solidity}}
}
" (hash "solidity" replaced-source))])
    (source-data sol-macro (first name) (flatten events))))

(define (mfn/gen name inputs body)
  (define formatted-inputs
    (map (lambda (input)
           (match input
             [(sfn-delta-edge from) from]
             [_ input]))
         inputs))

  (define -inputs
    (string-join
     (map (lambda (input)
            (match input
              [(sfn-delta-edge from) (format "~a: Deltas<DeltaProto<prost_wkt_types::Struct>>" from)]
              [_ (format "~a: prost_wkt_types::Struct" input)]))
          inputs)
     ","))

  (define initial-value
    (if (= (length formatted-inputs) 1)
        (format "let output_map = ~a;" (first formatted-inputs))
        (format "let output_map = (~a);" (string-join formatted-inputs ", "))))

  (define format-inputs (format "format_inputs!(~a);" (string-join formatted-inputs ",")))
  (expand
   "
#[substreams::handlers::map]
fn {{name}}({{inputs}}) -> Option<prost_wkt_types::Struct> {
    {{format-inputs}}
    with_map! {output_map,
      {{initial-value}}
      {{body}}
   }
}
"
   (hash "name"
         name
         "inputs"
         -inputs
         "format-inputs"
         format-inputs
         "body"
         body
         "initial-value"
         initial-value)))

(define (sfn/gen name inputs body attributes)
  (define -inputs
    (string-join (map (lambda (input) (format "~a: prost_wkt_types::Struct" input)) inputs) ","))

  (define initial-value
    (if (= (length inputs) 1)
        (format "let output_map = ~a;" (first inputs))
        (format "let output_map = (~a);" (string-join inputs ", "))))

  (define format-inputs (format "format_inputs!(~a);" (string-join inputs ",")))

  (define store-kind
    (if (member "immutable" attributes)
        "StoreSetIfNotExistsProto<prost_wkt_types::Struct>"
        "StoreSetProto<prost_wkt_types::Struct>"))

  (expand
   "
#[substreams::handlers::store]
fn {{name}}({{inputs}}, substreams_store_param: {{store-kind}}) {
    {{format-inputs}}
    {{initial-value}}
    {{body}}
}
"
   (hash "name"
         name
         "inputs"
         -inputs
         "format-inputs"
         format-inputs
         "body"
         body
         "initial-value"
         initial-value
         "store-kind"
         store-kind)))

(define (fmt-args args)
  (string-join (map (lambda (_) "SolidityType") args) ","))

(define (lam/gen fn-args exprs)
  (define -args (string-join fn-args ","))
  (define arg-types (fmt-args fn-args))
  (expand "
let output_map = (|({{args}}): ({{arg-types}})| { {{exprs}} })(output_map);
"
          (hash "args" -args "arg-types" arg-types "exprs" exprs)))

(define (hof/gen hof-kind fn-args exprs)
  (define -args (string-join fn-args ","))
  ; TODO I am leaving this here, because I might want the Option Type later
  ;let output_map:Option<SolidityType> = {{kind}}!(output_map, |{{args}}| { {{exprs}} });
  (expand "
  let output_map:SolidityType = {{kind}}!(output_map, |{{args}}| { {{exprs}} });
"
          (hash "kind" hof-kind "args" -args "exprs" exprs)))

(define (map-literal/gen kvs)
  (expand "
map_literal!{
{{kvs}}
}
" (hash "kvs" (string-join kvs ","))))

(define (key-value/gen key val)
  (expand "
\"{{key}}\"; {{val}}
" (hash "key" key "val" val)))

(define (binary-op/gen lh op rh)
  (format "SolidityType::from((~a ~a ~a))" lh op rh))

(define (field-access/gen lh rhs)
  (format "map_access!(&~a,~a)"
          lh
          (string-join (map (lambda (e)
                              (let ([key (match e
                                           [(number-literal val) val]
                                           [_ e])])
                                (format "\"~a\"" key)))
                            rhs)
                       ",")))

(define (rpc-call/gen name fn args)
  (expand-string "
{{name}}!({{fn}}Call, {{args}})
"
                 (hash "name"
                       name
                       "fn"
                       fn
                       "args"
                       (string-join (map (lambda (arg) (format "~a" (generate-code arg))) args)
                                    ","))))

(define (store-set/gen key value)
  (format "{substreams_store_param.generic_set(~a, ~a); SolidityType::Null}" key value))

(define (store-delete/gen prefix)
  (format "{substreams_store_param.generic_delete_prefix(~a); SolidityType::Null}" prefix))

(define (write-string-to-file string filename)
  (with-output-to-file filename (lambda () (pretty-display string)) #:exists 'replace))

(define (generate-code node)
  (match node
    [(source-def path) (gen source-def/gen path)]
    [(rpc-call instance fn args) (gen rpc-call/gen instance fn args)]
    [(map-literal kvs) (map-literal/gen (map generate-code kvs))]

    [(key-value key val) (gen key-value/gen key val)]
    [(identifier name) (gen symbol->string name)]
    [(binary-op lh op rh) (gen binary-op/gen lh op rh)]
    [(string-literal value) (format "sol_type!(String, \"~a\")" value)]
    [(number-literal value) (format "sol_type!(Uint, \"~a\")" value)]
    [(boolean-literal value) (format "sol_type!(Boolean, \"~a\")" (if value "1" "0"))]
    [(address-literal value) (format "sol_type!(Address, \"~a\")" value)]
    [(tuple-literal vals)
     (format "SolidityType::Tuple(vec![~a])" (string-join (map generate-code vals) ","))]
    [(list-literal vals)
     (format "SolidityType::List(vec![~a])" (string-join (map generate-code vals) ","))]
    [(mfn name inputs body _attributes) (gen mfn/gen name inputs body)]
    [(sfn name inputs body attributes) (gen sfn/gen name inputs body attributes)]
    [(store-set key value) (gen store-set/gen key value)]
    [(store-delete prefix) (gen store-delete/gen prefix)]
    [(lam fn-args exprs) (gen lam/gen fn-args exprs)]
    [(hof kind (lam args body)) (gen hof/gen kind args body)]
    [(pipeline functors) (string-join (map generate-code functors) "\n")]
    [(field-access lh rhs) (gen field-access/gen lh rhs)]
    [(? string?) node]
    [(? number?) node]
    [(list item ...)
     (string? item)
     node]))

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
  (define source-hash (make-hash (map (lambda (item) (rest item)) source-defs)))

  (define (instance-macro instance)
    (match-let ([(instance-def name abi address) instance])
      (expand-string
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
       (hash "name" name "abi" abi "address" (string-replace address "0x" "" #:all? #f)))))

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
         (lambda (instance)
           (let ([name (first instance)] [events (rest instance)])
             (expand
              "
        output_map.insert(\"{{name}}\", map_literal! {
{{{instance-events}}}
                    });
"
              (hash "name" name "instance-events" (string-join events)))))
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
