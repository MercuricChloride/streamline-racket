#lang racket/base

(require racket/match
         racket/list
         racket/bool
         racket/stream
         racket/set
         megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/functor
         data/applicative
         streamline/lang/lexer)

(struct module-data (kind name attributes) #:prefab)
(struct edge-data (from to mode) #:prefab)
(struct yaml-input-data (name kind mode) #:prefab)

;; PARAMETERS
(define declared-modules
  (make-parser-parameter (make-hash (list (cons "EVENTS" (module-data "MFN" "EVENTS" '()))
                                          (cons "BLOCK" (module-data "SOURCE" "BLOCK" '()))))))
(define declared-edges
  (make-parser-parameter (cons (edge-data "BLOCK" "EVENTS" "default")
                               '()))) ; This is a list of edges from module name -> module name

;; Used to modify how the attribute parsers operate
;; Because the attributes for sfns and mfns are all different
(define current-module-type (make-parser-parameter #f))

(define sfn-access-types `("deltas" "get"))

;; A tag attribute, is an attribute that doesn't have a value.
;; Ie the @immutable tag.
(define tag-attributes `("immutable"))
(define sfn-tag-attributes (set-subtract tag-attributes '()))
(define mfn-tag-attributes (set-subtract tag-attributes '("immutable")))

;; An value-attribute, is an attribute that does have a value
;; Ie the @startBlock tag.
;; @startBlock 12345
(define value-attributes `("startBlock"))
(define sfn-value-attributes (set-subtract value-attributes '()))
(define mfn-value-attributes (set-subtract value-attributes '()))

;; A key-value-attribute, is an attribute that has a key and a value
;; Ie the @const tag.
;; @const foo = bar
;; @var foo = bar
(define kv-attributes `("const" "var"))
(define sfn-kv-attributes (set-subtract kv-attributes '()))
(define mfn-kv-attributes (set-subtract kv-attributes '()))

;; STRUCTS
;;(struct primitive-value (value) #:prefab) ; IE Boolean etc
(struct identifier (name) #:prefab) ; foo
;(struct type (name) #:prefab) ; foo
;(struct typed-field (name type) #:prefab) ; id: address
(struct rpc-call (instance-name fn-name args) #:prefab) ; #bayc.ownerOf(id: address)#

(struct tag-attribute (name) #:prefab) ; @immutable
(struct value-attribute (name value) #:prefab) ; @startBlock 12345
(struct kv-attribute (name key value) #:prefab) ; @const foo = bar

(struct map-literal (kvs) #:prefab) ; { ... }
(struct var-literal (name) #:prefab) ; $foo
(struct string-literal (str) #:prefab) ; "hi"
(struct number-literal (num) #:prefab) ; 42
(struct boolean-literal (val) #:prefab) ; true / false
(struct address-literal (val) #:prefab) ; 0xabc123...
(struct tuple-literal (vals) #:prefab) ; (1, 2, 3)
(struct list-literal (vals) #:prefab) ; [1, 2, 3]
(struct binary-op (lh op rh) #:prefab) ; 42 + 5
(struct key-value (key value) #:prefab) ; foo: bar,
(struct field-access (lh fields) #:prefab) ; foo.bar or foo.bar.baz
(struct store-set (key value) #:prefab) ; set("hello", 42);
(struct store-delete (prefix) #:prefab) ; delete("hello");
(struct store-get (ident key) #:prefab) ; get(storeBids, "hello");
(struct do-block (expressions) #:prefab) ; do {...};
(struct function-call (name args) #:prefab) ; uint("12345")
(struct var-assignment (var value) #:prefab) ; foo = 42

(struct lam (args body) #:prefab) ; (foo) => bar;
(struct hof (hof callback) #:prefab) ; map (foo) => bar;
(struct pipeline (functors) #:prefab) ; |> (foo) => bar;

(struct mfn (name inputs body attributes) #:prefab) ; mfn foo = EVENTS...
(struct sfn (name inputs body attributes) #:prefab) ; sfn foo = EVENTS ...
(struct fn (name inputs body attributes) #:prefab) ; fn foo = [inputs] ...
(struct source-def (path) #:prefab) ; import "foo.sol";
(struct instance-def (name abi-type address) #:prefab) ; bayc = ERC721(0x...);

(struct sfn-delta-edge (name) #:prefab)

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

(define store-get/p
  (do (token/p 'GET)
      (token/p 'LPAREN)
      (identifier <- ident/p)
      (token/p 'COMMA)
      (key <- (lazy/p expression/p))
      (token/p 'RPAREN)
      (pure (store-get identifier key))))

(define store-set/p
  (do (token/p 'SET)
      (token/p 'LPAREN)
      (key <- (lazy/p expression/p))
      (token/p 'COMMA)
      (value <- (lazy/p expression/p))
      (token/p 'RPAREN)
      (pure (store-set key value))))

(define store-delete/p
  (do (token/p 'DELETE)
      (token/p 'LPAREN)
      (prefix <- (lazy/p expression/p))
      (token/p 'RPAREN)
      (pure (store-delete prefix))))

(define do-block/p
  (do (token/p 'DO)
      (token/p 'LCURLY)
      (exprs <- (many+/p (lazy/p expression/p) #:sep (token/p 'SEMI)))
      (token/p 'RCURLY)
      (pure (do-block exprs))))

(define group/p (do (token/p 'LPAREN) [expr <- (lazy/p expression/p)] (token/p 'RPAREN) (pure expr)))
(define ident/p (do [ident <- (syntax/p (token/p 'IDENTIFIER))] (pure ident)))
(define var/p (do (token/p 'AT) (ident <- ident/p) (pure (var-literal ident))))

(define literal/p
  (do [literal
       <-
       (or/p (try/p (lazy/p function-call/p))
             var/p
             group/p
             true/p
             false/p
             string-literal/p
             number-literal/p
             address-literal/p
             tuple-literal/p
             list-literal/p
             (lazy/p map-literal/p)
             ident/p)]
      (pure literal)))

(define type/p (do (ident <- ident/p) (pure ident)))

(define function-call-close/p
  (do (or/p (try/p (list/p (token/p 'COMMA) (token/p 'RPAREN))) (token/p 'RPAREN))))

(define function-call/p
  (do
   [function-name <- ident/p]
   (token/p 'LPAREN)
   ;; NOTE This returns a list of 2. The car of the list is the key-value/p match
   ;; And the cdr of the list is the match of the closing parser
   ;; We don't care about the end match, so we just return the car of the return
   [args <- (many+-until/p (lazy/p expression/p) #:end function-call-close/p #:sep (token/p 'COMMA))]
   (pure (function-call function-name (car args)))))

(define field-access-start/p (do [lh <- literal/p] (pure lh)))

(define field-access-end/p (do (token/p 'DOT) [rh <- literal/p] (pure rh)))

(define field-access/p
  (do [lh <- field-access-start/p]
      (rhs <- (many+/p field-access-end/p))
      (pure (field-access lh rhs))))

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

(define binary-op-start/p (do (lh <- (or/p (try/p field-access/p) literal/p)) (pure lh)))
(define binary-op-end/p
  (do [op <- binary-operators/p] [rh <- (or/p (try/p field-access/p) literal/p)] (pure (list op rh))))

(define binary-op/p
  (do [lh <- binary-op-start/p]
      [op <- binary-operators/p]
      [rh <- (or/p (try/p binary-op/p) binary-op-start/p)]
      (pure (binary-op lh op rh))))

(define map-closing/p
  (do (or/p (try/p (list/p (token/p 'COMMA) (token/p 'RCURLY))) (token/p 'RCURLY))))

(define map-literal/p
  (do (token/p 'LCURLY)
      ;; NOTE This returns a list of 2. The car of the list is the key-value/p match
      ;; And the cdr of the list is the match of map-closing/p.
      ;; We don't care about the end match, so we just return the car of the kvs in the final output
      [kvs <- (many+-until/p key-value/p #:end map-closing/p #:sep (token/p 'COMMA))]
      (pure (map-literal (car kvs)))))

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

;; TODO I need to add a guard to this to make sure variables are in scope
(define var-assignment/p
  (do (var <- ident/p)
      (token/p 'ASSIGNMENT)
      (value <- (lazy/p expression/p))
      (pure (var-assignment var value))))

(define expression/p
  (or/p (try/p binary-op/p)
        store-set/p
        store-delete/p
        store-get/p
        (try/p function-call/p)
        (try/p var-assignment/p)
        do-block/p
        rpc-call/p
        (try/p field-access/p)
        literal/p))

(define lambda/p
  (do (token/p 'LPAREN)
      [fn-args <- (many/p ident/p #:sep (token/p 'COMMA))]
      (token/p 'RPAREN)
      (token/p 'FAT-ARROW)
      [expr <- expression/p]
      (pure (lam fn-args expr))))

(define map/p (do (token/p 'MAP) [callback <- lambda/p] (pure (hof "map" callback))))

(define filter/p (do (token/p 'FILTER) [callback <- lambda/p] (pure (hof "filter" callback))))

(define functor/p (do (token/p 'PIPE) [f <- (or/p map/p filter/p lambda/p)] (token/p 'SEMI) (pure f)))

(define pipeline/p (do [applications <- (many/p functor/p #:min 1)] (pure (pipeline applications))))

(define store-access-type/p
  (do
   (modules <- (declared-modules))
   (input <- ident/p)
   (datum-input <- (pure (syntax->datum input)))
   (token/p 'DOT)
   (mode
    <-
    (guard/p
     ident/p
     (lambda (mode)
       (let* ([mode (syntax->datum mode)])
         (match (hash-ref modules datum-input)
           [(module-data "SFN" _ _) (if (member mode sfn-access-types) true false)]
           [_ false])))
     #f
     (lambda (mode)
       (let ([module-kind (module-data-kind (hash-ref modules datum-input))]
             [input-mode (syntax->datum mode)])
         (cond
           [(not (equal? module-kind "SFN"))
            "Invalid module type for! Please use an SFN if using <MODULE>.deltas or <MODULE>.get!"]
           [(not (member input-mode sfn-access-types))
            "Invalid special access kind! Valid field access on an SFN is `<MODULE>.deltas` or `<MODULE>.get`"]
           [else
            "This should never show up! If it does, go reach out to @blind_nabler and tell him his code is broken! Sorry!"])))))
   (pure (match mode
           ["deltas" (sfn-delta-edge input)]
           [_ input]))))

(define module-input/p (do [input <- (or/p (try/p store-access-type/p) ident/p)] (pure input)))

(define many-module-inputs/p
  (do (token/p 'LBRACKET)
      [inputs <- (many/p module-input/p #:min 1 #:sep (token/p 'COMMA))]
      (token/p 'RBRACKET)
      (pure inputs)))

(define single-module-input/p (do [input <- module-input/p] (pure (list input))))

(define module-inputs/p
  (do (inputs <- (or/p (try/p many-module-inputs/p) single-module-input/p)) (pure inputs)))

(define module-name/p
  (do (name <-
            (guard/p ident/p
                     (lambda (name) (not (equal? name "EVENTS")))
                     "EVENTS is a restricted module name. Please choose a different name!"))
      (pure name)))

(define valid-tag-attribute/p
  (do [module-type <- (current-module-type)]
      [ident
       <-
       (guard/p
        ident/p
        (λ (ident)
          (match module-type
            ["MFN" (member ident mfn-tag-attributes)]
            ["SFN" (member ident sfn-tag-attributes)]))
        #f
        (λ (ident)
          (match module-type
            ["MFN"
             (format "Invalid tag attribute ~a. Please use one of the following for MFN modules: ~a"
                     ident
                     mfn-tag-attributes)]
            ["SFN"
             (format "Invalid tag attribute ~a. Please use one of the following for SFN modules: ~a"
                     ident
                     sfn-tag-attributes)])))]
      (pure ident)))

(define valid-value-attribute/p
  (do [module-type <- (current-module-type)]
      [ident
       <-
       (guard/p
        ident/p
        (λ (ident)
          (match module-type
            ["MFN" (member ident mfn-value-attributes)]
            ["SFN" (member ident sfn-value-attributes)]))
        #f
        (λ (ident)
          (match module-type
            ["MFN"
             (format "Invalid value attribute. Please use one of the following for MFN modules: ~a"
                     mfn-value-attributes)]
            ["SFN"
             (format "Invalid value attribute. Please use one of the following for SFN modules: ~a"
                     sfn-value-attributes)])))]
      (pure ident)))

(define valid-kv-attribute/p
  (do [module-type <- (current-module-type)]
      [ident
       <-
       (guard/p
        ident/p
        (λ (ident)
          (match module-type
            ["MFN" (member ident mfn-kv-attributes)]
            ["SFN" (member ident sfn-kv-attributes)]))
        #f
        (λ (ident)
          (match module-type
            ["MFN"
             (format
              "Invalid key-value attribute ~a. Please use one of the following for MFN modules: ~a"
              ident
              mfn-kv-attributes)]
            ["SFN"
             (format
              "Invalid key-value attribute ~a. Please use one of the following for SFN modules: ~a"
              ident
              sfn-kv-attributes)])))]
      (pure ident)))

(define tag-attribute/p
  (do (tag <-
           (guard/p ident/p
                    (λ (ident) (member (syntax->datum ident) tag-attributes))
                    #f
                    (λ (ident)
                      (format "Invalid tag attribute: ~a, use one of the following! ~a"
                              ident
                              tag-attributes))))
      (pure (tag-attribute tag))))

(define value-attribute/p
  (do (name <-
            (guard/p ident/p
                     (λ (ident) (member (syntax->datum ident) value-attributes))
                     #f
                     (λ (ident)
                       (format "Invalid value attribute: ~a, use one of the following! ~a"
                               ident
                               value-attributes))))
      (value <- (or/p (try/p store-access-type/p) literal/p))
      (pure (value-attribute name value))))

(define kv-attribute/p
  (do (name <-
            (guard/p ident/p
                     (λ (ident) (member (syntax->datum ident) kv-attributes))
                     #f
                     (λ (ident)
                       (format "Invalid key value attribute: ~a, use one of the following! ~a"
                               ident
                               kv-attributes))))
      (key <- ident/p)
      (token/p 'ASSIGNMENT)
      (value <- (or/p (try/p store-access-type/p) literal/p))
      (pure (kv-attribute name key value))))

(define attribute/p
  (do (token/p 'AT)
      (attribute <- (or/p (try/p kv-attribute/p) (try/p value-attribute/p) (try/p tag-attribute/p)))
      (pure attribute)))

;; (define attribute/p
;;   (do (attr <- (or/p (try/p kv-attribute/p) (try/p value-attribute/p) (try/p tag-attribute/p)))
;;       (pure attr)))

(define mfn-lookahead/p (lookahead/p (do (attributes <- (many*/p attribute/p)) (token/p 'MFN))))
(define (mfn/p attributes)
  (do (token/p 'MFN)
      [modules <- (declared-modules)]
      [edges <- (declared-edges)]
      [name
       <-
       (guard/p module-name/p
                (λ (name) (not (hash-has-key? modules name)))
                "This module name already has been defined!")]
      [datum-name <- (pure (syntax->datum name))]
      [datum-attributes <- (pure (syntax->datum attributes))]
      (declared-modules (let ([ht (hash-copy modules)])
                          (hash-set! ht datum-name (module-data "MFN" datum-name '()))
                          ht))
      (token/p 'ASSIGNMENT)
      [inputs
       <-
       (guard/p module-inputs/p
                (lambda (inputs)
                  (for/and ([input inputs])
                    (let* ([input (syntax->datum input)])
                      (match input
                        [(sfn-delta-edge from) (hash-has-key? modules from)]
                        [_ (hash-has-key? modules input)]))))
                "This module name is undefined! Please use a defined module for inputs to a module!")]
      (declared-edges
       (stream-append edges
                      (map (lambda (input)
                             (match input
                               [(sfn-delta-edge from)
                                (edge-data (syntax->datum from) (syntax->datum name) "deltas")]
                               [_ (edge-data (syntax->datum input) (syntax->datum name) "default")]))
                           inputs)))
      [pipeline <- pipeline/p]
      (pure (mfn name inputs pipeline datum-attributes))))

(define (sfn/p attributes)
  (do (token/p 'SFN)
      [modules <- (declared-modules)]
      [edges <- (declared-edges)]
      [name
       <-
       (guard/p ident/p
                (lambda (name) (not (hash-has-key? modules name)))
                "This module name already has been defined!")]
      [datum-name <- (pure (syntax->datum name))]
      [datum-attributes <- (pure (syntax->datum attributes))]
      (declared-modules (let ([ht (hash-copy modules)])
                          (hash-set! ht datum-name (module-data "SFN" datum-name datum-attributes))
                          ht))
      (token/p 'ASSIGNMENT)
      [inputs
       <-
       (guard/p module-inputs/p
                (lambda (inputs)
                  (for/and ([input inputs])
                    (let* ([input (syntax->datum input)])
                      (match input
                        [(sfn-delta-edge from) (hash-has-key? modules from)]
                        [_ (hash-has-key? modules input)]))))
                "This module name is undefined! Please use a defined module for inputs to a module!")]
      (declared-edges
       (stream-append edges
                      (map (lambda (input)
                             (match input
                               [(sfn-delta-edge from)
                                (edge-data (syntax->datum from) (syntax->datum name) "deltas")]
                               [_ (edge-data (syntax->datum input) datum-name "default")]))
                           inputs)))
      [pipeline <- pipeline/p]
      (pure (sfn name inputs pipeline datum-attributes))))

(define (fn/p attributes)
  (do (token/p 'FN)
      [modules <- (declared-modules)]
      [name
       <-
       (guard/p ident/p
                (lambda (name) (not (hash-has-key? modules name)))
                "This function name already has been defined!")]
      [datum-name <- (pure (syntax->datum name))]
      [datum-attributes <- (pure (syntax->datum attributes))]
      (declared-modules (let ([ht (hash-copy modules)])
                          (hash-set! ht datum-name (module-data "FN" datum-name datum-attributes))
                          ht))
      (token/p 'ASSIGNMENT)
      [inputs <- module-inputs/p]
      [pipeline <- pipeline/p]
      (pure (fn name inputs pipeline datum-attributes))))

(define module-def/p
  (do (attributes <- (syntax/p (many*/p attribute/p)))
      (mod <- (syntax/p (or/p (fn/p attributes) (mfn/p attributes) (sfn/p attributes))))
      (pure mod)))

(define source-def/p
  (do (token/p 'SOURCE)
      (path <- (syntax/p (token/p 'STRING)))
      (token/p 'SEMI)
      (pure (source-def path))))

(define instance-def/p
  (do (name <- (syntax/p ident/p))
      (token/p 'ASSIGNMENT)
      (abi-type <- (syntax/p ident/p))
      (token/p 'LPAREN)
      (address <- (syntax/p (token/p 'ADDRESS)))
      (token/p 'RPAREN)
      (token/p 'SEMI)
      (pure (instance-def name abi-type address))))

(define streamline/p
  (do [cells <- (syntax/p (many/p (or/p module-def/p source-def/p instance-def/p)))]
      [modules <- (declared-modules)]
      [edges <- (declared-edges)]
      (pure (list cells modules edges))))

(define (module-input->yaml input)
  (define kind (car input))
  (define name (cdr input))

  (define yaml (make-hash))

  (match kind
    ["MFN" (hash-set! yaml "map" name)]
    ["SFN" (hash-set! yaml "store" name)]
    ["SOURCE" (hash-set! yaml "source" "sf.ethereum.type.v2.Block")])
  yaml)

(define (format-yaml-input input)
  (match input
    [(yaml-input-data name "MFN" "default") (hash "map" name)]
    [(yaml-input-data name "SFN" "default") (hash "store" name "mode" "get")]
    [(yaml-input-data name "SFN" "deltas") (hash "store" name "mode" "deltas")]
    [(yaml-input-data name "SOURCE" "default") (hash "source" "sf.ethereum.type.v2.Block")]
    [(yaml-input-data name "FN" _) null]))

(define (module->yaml mod inputs)
  (let ([output (hash "type" "proto:google.protobuf.Struct")]
        [inputs (stream->list (map format-yaml-input inputs))])
    (match mod
      [(module-data "MFN" name attributes)
       (hash "name" name "kind" "map" "output" output "inputs" inputs "initialBlock" 18812993)]

      [(module-data "SFN" name attributes)
       (hash "name"
             name
             "updatePolicy"
             (if (member "immutable" attributes) "set_if_not_exists" "set")
             "kind"
             "store"
             "valueType"
             "proto:google.protobuf.Struct"
             "inputs"
             inputs
             "initialBlock"
             18812993)]
      [_ #f])))

(define (parse-file! tokenized-input)
  (define result (parse-result! (parse-tokens streamline/p tokenized-input)))
  (match result
    [(list parsed-file modules edges)
     (begin
       (define nodes
         (filter-map (lambda (mod)
                       (define inputs
                         ;; Find modules inputs
                         (filter-map (lambda (edge)
                                       (define module-name (module-data-name mod))
                                       (if (equal? (edge-data-to edge) module-name)
                                           (let* ([from-module (hash-ref modules
                                                                         (edge-data-from edge))]
                                                  [from-name (module-data-name from-module)]
                                                  [from-kind (module-data-kind from-module)]
                                                  [mode (edge-data-mode edge)])
                                             (yaml-input-data from-name from-kind mode))
                                           #f))
                                     (stream->list edges)))
                       (module->yaml mod (stream->list inputs)))
                     (hash-values modules)))
       (hash "parsed-file" parsed-file "nodes" (stream->list nodes)))]))

(define (parse-streamline! input-port)
  (define tokenized-input (tokenize input-port))
  (hash-ref (parse-file! tokenized-input) "parsed-file"))

(provide parse-streamline!
         parse-file!
         ;;primitive-value
         identifier
         ;type
         ;typed-field
         rpc-call

         do-block
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
         store-set
         store-delete
         store-get
         function-call
         var-assignment
         var-literal

         tag-attribute
         value-attribute
         kv-attribute

         sfn-delta-edge
         yaml-input-data

         mfn
         sfn
         fn
         source-def
         instance-def)