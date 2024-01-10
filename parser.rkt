#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/functor
         data/applicative
         dali
         "./lexer.rkt")

;; PARAMETERS
(define declared-modules
  (make-parser-parameter
   '(("MFN" . "EVENTS") ("SOURCE" .
                                  "BLOCK")))) ; This is a list of pairs of (MODULE_TYPE . MODULE_NAME)
(define declared-edges
  (make-parser-parameter
   '(("BLOCK" . "EVENTS")))) ; This is a list of edges from module name -> module name

(define sfn-access-types `("deltas" "get"))

;; STRUCTS
(struct primitive-value (value) #:prefab) ; IE Boolean etc
(struct identifier (name) #:prefab) ; foo
(struct type (name) #:prefab) ; foo
(struct typed-field (name type) #:prefab) ; id: address
(struct rpc-call (instance-name fn-name args) #:prefab) ; ERC721.#ownerOf(id: address)#

(struct map-literal (kvs) #:prefab) ; { ... }
(struct string-literal (str) #:prefab) ; "hi"
(struct number-literal (num) #:prefab) ; 42
(struct boolean-literal (val) #:prefab) ; true / false
(struct address-literal (val) #:prefab) ; 0xabc123...
(struct tuple-literal (vals) #:prefab) ; (1, 2, 3)
(struct list-literal (vals) #:prefab) ; [1, 2, 3]
(struct binary-op (lh op rh) #:prefab) ; 42 + 5
(struct key-value (key value) #:prefab) ; foo: bar,
(struct field-access (lh fields) #:prefab) ; foo.bar or foo.bar.baz

(struct lam (args body) #:prefab) ; (foo) => bar;
(struct hof (hof callback) #:prefab) ; map (foo) => bar;
(struct pipeline (functors) #:prefab) ; |> (foo) => bar;

(struct mfn (name inputs body) #:prefab) ; mfn foo = EVENTS...
(struct sfn (name inputs body) #:prefab) ; sfn foo = EVENTS ...
(struct source-def (path) #:prefab) ; import "foo.sol";
(struct instance-def (name abi-type address) #:prefab) ; bayc = ERC721(0x...);

;; EXPRESSIONS
(define true/p (do (token/p 'TRUE) (pure (boolean-literal #t))))
(define false/p (do (token/p 'FALSE) (pure (boolean-literal #f))))
(define string-literal/p (do (str <- (token/p 'STRING)) (pure (string-literal str))))
(define number-literal/p (do (num <- (token/p 'NUMBER)) (pure (number-literal num))))
(define address-literal/p (do (addr <- (token/p 'ADDRESS)) (pure (address-literal addr))))
(define tuple-literal/p
  (do (token/p 'LPAREN)
      [vals <- (many+/p (lazy/p expression/p) #:sep (token/p 'COMMA))]
      (token/p 'RPAREN)
      (pure (tuple-literal vals))))
(define list-literal/p
  (do (token/p 'LBRACKET)
      [vals <- (many+/p (lazy/p expression/p) #:sep (token/p 'COMMA))]
      (token/p 'RBRACKET)
      (pure (list-literal vals))))

(define literal/p
  (do [literal
       <-
       (or/p true/p
             false/p
             string-literal/p
             number-literal/p
             address-literal/p
             tuple-literal/p
             list-literal/p)]
      (pure literal)))

(define ident/p (do [ident <- (token/p 'IDENTIFIER)] (pure ident)))
(define type/p (do (ident <- ident/p) (pure ident)))

(define (flatten-field-access rh)
  (match rh
    [(field-access lh rh) (flatten (list lh (flatten-field-access rh)))]
    [_ (list rh)]))

(define field-access/p
  (do [lh <- (or/p ident/p literal/p)]
      (token/p 'DOT)
      (rh <- (or/p (try/p field-access/p) ident/p number-literal/p))
      (pure (field-access lh (flatten-field-access rh)))))

(define key-value/p
  (do [key <- ident/p] (token/p 'COLON) [val <- (lazy/p expression/p)] (pure (key-value key val))))

(define binary-operators/p
  (do (op <-
          (or/p (token/p 'PLUS)
                (token/p 'MINUS)
                (token/p 'MUL)
                (token/p 'DIV)
                (token/p 'EQ)
                (token/p 'NOT-EQ)
                (token/p 'LT)
                (token/p 'GT)
                (token/p 'LTE)
                (token/p 'GTE)
                (token/p 'AND)
                (token/p 'OR)
                (token/p 'NOT)))
      (pure op)))

(define binary-op/p
  (do [lh <- (or/p literal/p ident/p (try/p field-access/p))]
      [op <- binary-operators/p]
      [rh <- (lazy/p expression/p)]
      (pure (binary-op lh op rh))))

(define map-literal/p
  (do (token/p 'LCURLY)
      [kvs <- (many/p key-value/p #:min 1 #:sep (token/p 'COMMA))]
      (many/p #:max 1 (token/p 'COMMA))
      (token/p 'RCURLY)
      (pure (map-literal kvs))))

(define typed-field/p
  (do (ident <- ident/p) (token/p 'COLON) (type <- type/p) (pure (typed-field ident type))))

; TODO I need to add loading instances at an address
(define rpc-call/p
  (do (token/p 'HASH)
      (instance-name <- ident/p)
      (token/p 'DOT)
      (fn-name <- ident/p)
      (token/p 'LPAREN)
      (args <- (many/p expression/p))
      (token/p 'RPAREN)
      (token/p 'HASH)
      (pure (rpc-call instance-name fn-name args))))

(define expression/p
  (or/p (try/p binary-op/p)
        rpc-call/p
        map-literal/p
        true/p
        false/p
        string-literal/p
        number-literal/p
        address-literal/p
        (try/p field-access/p)
        tuple-literal/p
        list-literal/p
        ident/p))

(define hof/p (or/p (token/p 'MAP) (token/p 'FILTER) (token/p 'REDUCE)))

(define lambda/p
  (do (token/p 'LPAREN)
      [fn-args <- (many/p ident/p #:min 1 #:sep (token/p 'COMMA))]
      (token/p 'RPAREN)
      (token/p 'FAT-ARROW)
      [exprs <- expression/p]
      (pure (lam fn-args exprs))))

(define map/p (do (token/p 'MAP) [callback <- lambda/p] (pure (hof "map" callback))))

(define filter/p (do (token/p 'FILTER) [callback <- lambda/p] (pure (hof "filter" callback))))

(define functor/p (do (token/p 'PIPE) [f <- (or/p map/p filter/p lambda/p)] (token/p 'SEMI) (pure f)))

(define pipeline/p (do [applications <- (many/p functor/p #:min 1)] (pure (pipeline applications))))

(define store-deltas/p
  (let ([guard-return ""])
    (do
     (modules <- (declared-modules))
     (input <- ident/p)
     (token/p 'DOT)
     (mode
      <-
      (guard/p
       ident/p
       (lambda (mode)
         (let ([module-kind (car (find-module input modules))])
           (cond
             [(not (equal? module-kind "SFN")) false]
             [(not (member mode sfn-access-types)) false]
             [else true])))
       #f
       (lambda (mode)
         (let ([module-kind (car (find-module input modules))])
           (cond
             [(not (equal? module-kind "SFN"))
              "Invalid module type for! Please use an SFN if using <MODULE>.deltas or <MODULE>.get!"]
             [(not (member mode sfn-access-types))
              "Invalid special access kind! Valid field access on an SFN is `<MODULE>.deltas` or `<MODULE>.get`"]
             [else
              "This should never show up! If it does, go reach out to @blind_nabler and tell him his code is broken! Sorry!"])))))
     ;; TODO I need to store the input as a delta still
     (pure input))))

(define module-input/p (do [input <- (or/p (try/p store-deltas/p) ident/p)] (pure input)))

(define many-module-inputs/p
  (do (token/p 'LBRACKET)
      [inputs <- (many/p module-input/p #:min 1 #:sep (token/p 'COMMA))]
      (token/p 'RBRACKET)
      (pure inputs)))

(define single-module-input/p (do [inputs <- module-input/p] (pure (list inputs))))

(define module-inputs/p
  (do (inputs <- (or/p (try/p many-module-inputs/p) single-module-input/p)) (pure inputs)))

(define module-name/p
  (do (name <-
            (guard/p ident/p
                     (lambda (name) (not (equal? name "EVENTS")))
                     "EVENTS is a restricted module name. Please choose a different name!"))
      (pure name)))

(define mfn/p
  (do (token/p 'MFN)
      [modules <- (declared-modules)]
      [edges <- (declared-edges)]
      [name
       <-
       (guard/p module-name/p
                (lambda (name) (not (member name (stream->list (map cdr modules)))))
                "This module name already has been defined!")]
      (declared-modules (cons (cons "MFN" name) modules))
      (token/p 'ASSIGNMENT)
      [inputs
       <-
       (guard/p module-inputs/p
                (lambda (inputs)
                  (for/and ([input inputs])
                    (member input (stream->list (map cdr modules)))))
                "This module name is undefined! Please use a defined module for inputs to a module!")]
      (declared-edges (stream-append edges (map (lambda (input) (cons input name)) inputs)))
      [pipeline <- pipeline/p]
      (pure (mfn name inputs pipeline))))

(define sfn/p
  (do (token/p 'SFN)
      [modules <- (declared-modules)]
      [edges <- (declared-edges)]
      [name
       <-
       (guard/p ident/p
                (lambda (name) (not (member name modules)))
                "This module name already has been defined!")]
      (declared-modules (cons (cons "SFN" name) modules))
      (token/p 'ASSIGNMENT)
      [inputs
       <-
       (guard/p module-inputs/p
                (lambda (inputs)
                  (for/and ([input inputs])
                    (member input (stream->list (map cdr modules)))))
                "This module name is undefined! Please use a defined module for inputs to a module!")]
      (declared-edges (stream-append edges (map (lambda (input) (cons input name)) inputs)))
      [pipeline <- pipeline/p]
      (pure (sfn name inputs pipeline))))

(define module-def/p (or/p mfn/p sfn/p))

(define source-def/p
  (do (token/p 'SOURCE) (path <- (token/p 'STRING)) (token/p 'SEMI) (pure (source-def path))))

(define instance-def/p
  (do (name <- ident/p)
      (token/p 'ASSIGNMENT)
      (abi-type <- ident/p)
      (token/p 'LPAREN)
      (address <- (token/p 'ADDRESS))
      (token/p 'RPAREN)
      (token/p 'SEMI)
      (pure (instance-def name abi-type address))))

(define streamline/p
  (do [cells <- (many/p (or/p module-def/p source-def/p instance-def/p))]
      [modules <- (declared-modules)]
      [edges <- (declared-edges)]
      (pure (list cells modules (stream->list edges)))))

(define (find-module name modules)
  (define filtered (filter (lambda (mod) (equal? name (cdr mod))) (stream->list modules)))
  (if (empty? filtered) false (first filtered)))

(define (module-input->yaml input)
  (define kind (car input))
  (define name (cdr input))

  (define yaml (make-hash))

  (match kind
    ["MFN" (hash-set! yaml "map" name)]
    ["SFN" (hash-set! yaml "store" name)]
    ["SOURCE" (hash-set! yaml "source" "sf.ethereum.type.v2.Block")])
  yaml)

(define (module->yaml mod inputs)
  (if (equal? (car mod) "SOURCE")
      false
      (let ([output (hash "type" "proto:google.protobuf.Struct")]
            [store? (match (car mod)
                      ["MFN" false]
                      ["SFN" true])]
            [yaml (make-hash)])

        (hash-set! yaml "name" (cdr mod))
        (hash-set! yaml "kind" (if store? "store" "map"))
        (hash-set! yaml "inputs" (stream->list (map module-input->yaml inputs)))

        (when (not store?) ; only map modules have outputs
          (hash-set! yaml "output" output))
        (when store?
          (hash-set! yaml "valueType" "proto:google.protobuf.Struct")
          (hash-set! yaml "updatePolicy" "set"))

        yaml)))

(define (parse-file! tokenized-input)
  (match (parse-result! (parse-tokens streamline/p tokenized-input))
    [(list parsed-file modules edges)
     (begin
       (define nodes
         (filter-map
          (lambda (mod)
            (define inputs
              (filter-map (lambda (edge)
                            (define module-name (cdr mod))
                            (if (equal? (cdr edge) module-name) (find-module (car edge) modules) #f))
                          edges))
            (module->yaml mod (stream->list inputs)))
          modules))
       (hash "parsed-file"
             parsed-file
             "modules"
             modules
             "edges"
             edges
             "nodes"
             (stream->list nodes)))]))

(provide parse-file!
         primitive-value
         identifier
         type
         typed-field
         rpc-call

         map-literal
         string-literal
         number-literal
         boolean-literal
         address-literal
         tuple-literal
         list-literal
         binary-op
         key-value
         field-access
         lam
         hof
         pipeline

         mfn
         sfn
         source-def
         instance-def)
