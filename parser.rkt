#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/functor
         data/applicative
         dali
         "./lexer.rkt")

;; STRUCTS
(struct primitive-value (value) #:prefab) ; IE Boolean etc
(struct identifier (name) #:prefab) ; foo
(struct type (name) #:prefab) ; foo
(struct typed-field (name type) #:prefab) ; id: address
(struct rpc-call (fn-access typed-args) #:prefab) ; ERC721::#ownerOf(id: address)#

(struct map-literal (kvs) #:prefab) ; { ... }
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
(define true/p (do (token/p 'TRUE) (pure `(boolean ,#t))))
(define false/p (do (token/p 'FALSE) (pure `(boolean ,#f))))
(define string-literal/p (do (str <- (token/p 'STRING)) (pure str)))
(define number-literal/p (do (num <- (token/p 'NUMBER)) (pure num)))

(define ident/p (do [ident <- (token/p 'IDENTIFIER)] (pure ident)))
(define type/p (do (ident <- ident/p) (pure ident)))

(define (flatten-field-access rh)
  (match rh
    [(field-access lh rh) (flatten (list lh (flatten-field-access rh)))]
    [_ (list rh)]))

(define field-access/p
  (do [lh <- ident/p]
      (token/p 'DOT)
      (rh <- (or/p (try/p field-access/p) ident/p))
      (pure (field-access lh (flatten-field-access rh)))))

(define key-value/p
  (do [key <- ident/p] (token/p 'COLON) [val <- (lazy/p expression/p)] (pure (key-value key val))))

(define map-literal/p
  (do (token/p 'LCURLY)
      [kvs <- (many/p key-value/p #:min 1 #:sep (token/p 'COMMA))]
      (token/p 'RCURLY)
      (pure (map-literal kvs))))

(define typed-field/p
  (do (ident <- ident/p) (token/p 'COLON) (type <- type/p) (pure (typed-field ident type))))

(define rpc-call/p
  (do (token/p 'HASH)
      (fn-access <- (or/p field-access/p ident/p))
      (token/p 'LPAREN)
      (typed-args <- (many/p typed-field/p))
      (token/p 'RPAREN)
      (token/p 'HASH)
      (pure (rpc-call fn-access typed-args))))

(define expression/p
  (or/p rpc-call/p
        map-literal/p
        true/p
        false/p
        string-literal/p
        number-literal/p
        (try/p field-access/p)
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

(define mfn/p
  (do (token/p 'MFN)
      [name <- ident/p]
      (token/p 'ASSIGNMENT)
      [inputs <- (many/p ident/p #:min 1 #:sep (token/p 'SEMI))]
      [pipeline <- pipeline/p]
      (pure (mfn name inputs pipeline))))

(define sfn/p
  (do (token/p 'SFN)
      [name <- (token/p 'IDENTIFIER)]
      (token/p 'ASSIGNMENT)
      [inputs <- (many/p ident/p #:min 1 #:sep (token/p 'SEMI))]
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
  (do [cells <- (many/p (or/p module-def/p source-def/p instance-def/p))] (pure cells)))

(define (parse-file! tokenized-input)
  (parse-result! (parse-tokens streamline/p tokenized-input)))

(provide parse-file!
         primitive-value
         identifier
         type
         typed-field
         rpc-call

         map-literal
         key-value
         field-access
         lam
         hof
         pipeline

         mfn
         sfn
         source-def
         instance-def)
