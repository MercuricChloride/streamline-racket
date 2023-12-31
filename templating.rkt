#lang racket

(require json)
(require dali)
(require "./lexer.rkt")
(require "./parser.rkt")

(define-syntax-rule (gen proc args ...) (apply proc (map generate-code (list args ...))))

;; Data for generating contract source code
(struct source-data (sol-macro name events))

(define (fn-inputs input)
  (expand-string "{{#inputs}} {{ident}}: {{type}} {{/inputs}}" input))

(define (fn-template input)
  (expand-string
   "
   #[substreams::handlers::map]
   fn {{fn-name}}({{inputs}}) -> prost_wkt_types::Struct {
       {{expression}}
   }"
   input))

(define-syntax-rule (source path)
  (let* ([source-code (port->string (open-input-file path))]
         [events
          (regexp-match* #rx"event ([a-zA-Z$_][a-zA-Z0-9$_]*)" source-code #:match-select cadr)]
         [name (regexp-match* #rx"(?:interface|contract) ([a-zA-Z$_][a-zA-Z0-9$_]*)"
                              source-code
                              #:match-select cadr)]
         [sol-macro (expand-string "
loose_sol! {
  {{solidity}}
}
" (hash "solidity" source-code))])
    (contract-instance sol-macro name events)))

(define (get-events/gen event-name abi address)
  (expand-string
   "
    \"{{event-name}}\"; {{abi}}::{{event-name}}::get_events(&blk, &[&address!(\"{{address}}\")])
"
   (hash "event-name" event-name "abi" abi "address" (string-downcase (substring address 2)))))

(define (instance-def/gen name identifier address)
  (expand-string
   "
        map_insert!(\"{{name}}\",
                    map_literal! {
                        \"Transfer\"; {{identifier}}::Transfer::get_events(&blk, &[address!(\"{{address}}\")])
                    },
                    output_map);
"
   (hash "name" name "identifier" identifier "address" "asfd")))

(define (sources parser-result)
  (filter (lambda (item) (eq? (first item) 'source)) parser-result))

(define (instances parser-result)
  (filter (lambda (item) (eq? (first item) 'instance)) parser-result))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (source-def/gen path)
  (let* ([source-code (port->string (open-input-file path))]
         [events
          (regexp-match* #rx"event ([a-zA-Z$_][a-zA-Z0-9$_]*)" source-code #:match-select cadr)]
         [name (regexp-match* #rx"(?:interface|contract) ([a-zA-Z$_][a-zA-Z0-9$_]*)"
                              source-code
                              #:match-select cadr)]
         [sol-macro (expand-string "
loose_sol! {
  {{solidity}}
}
" (hash "solidity" source-code))])
    (source-data sol-macro (first name) (flatten events))))

(define (mfn/gen name inputs body)
  (define -inputs
    (string-join (map (lambda (input) (format "~a: prost_wkt_types::Struct" input)) inputs) ","))
  (define format-inputs (format "format_inputs!(~a);" (string-join inputs ",")))
  (expand-string
   "
#[substreams::handlers::map]
fn {{name}}({{inputs}}) -> prost_wkt_types::Struct {
    {{format-inputs}}
    with_map! {output_map,
      {{body}}
   }
}
"
   (hash "name" name "inputs" -inputs "format-inputs" format-inputs "body" body)))

(define (sfn/gen name inputs body)
  (define -inputs
    (string-join (map (lambda (input) (format "~a: prost_wkt_types::Struct" input)) inputs) ","))
  (define format-inputs (format "format_inputs!(~a);" (string-join inputs ",")))
  (expand-string
   "
#[substreams::handlers::map]
fn {{name}}({{inputs}}) -> prost_wkt_types::Struct {
    {{format-inputs}}
    with_map! {output_map,
      {{body}}
   }
}
"
   (hash "name" name "inputs" -inputs "format-inputs" format-inputs "body" body)))

(define (write-string-to-file string filename)
  (with-output-to-file filename (lambda () (display string)) #:exists 'replace))

(define (generate-code node)
  (match node
    [(source-def path) (gen source-def/gen path)]
    [(instance-def name identifier address) (gen instance-def/gen name identifier address)]
    [(identifier name) (gen symbol->string name)]
    [(mfn name inputs body) (gen mfn/gen name inputs body)]
    [(sfn name inputs body) (gen sfn/gen name inputs body)]
    [(pipeline functors) (format "~a" functors)]
    [(? string?) node]
    [(? number?) node]
    [(list item ...)
     (string? item)
     node]))

(define (generate-streamline-file path)
  ; open a port to the source
  (define source-port (open-input-file path))
  (println "Opened source port")

  ; lex the file
  (define tokenized-input (tokenize source-port))
  (println "Tokenized Input")

  ; parse the file
  (define parsed-input (parse-file! tokenized-input))
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
fn map_events(blk: eth::Block) -> prost_wkt_types::Struct {
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
             (expand-string
              "
        map_insert!(\"{{name}}\",
                    map_literal! {
{{{instance-events}}}
                    },
                    output_map);
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
      (string-append source-def-code instances-def-code module-code)))
  (println "Generated Code")
  (write-string-to-file generated-code "/tmp/streamline.rs")
  (println source-hash)
  (println "Wrote output code"))

(generate-streamline-file "examples/erc721.strm")
