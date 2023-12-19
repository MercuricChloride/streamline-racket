#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/functor
         data/applicative
         "./lexer.rkt")
;; EXPRESSIONS

(define true/p (do (token/p 'TRUE) (pure (cons 'boolean #t))))

(define false/p (do (token/p 'FALSE) (pure (cons 'boolean #f))))

(define ident/p (do [ident <- (token/p 'IDENTIFIER)] (pure (cons 'identifier ident))))
(define type/p (do (ident <- ident/p) (pure (cons 'type ident))))

(define field-access/p
  (do [lh <- ident/p]
      (token/p 'DOT)
      (rh <- (or/p (try/p field-access/p) ident/p))
      (pure `(field-access ,lh ,rh))))

(define typed-field/p
  (do (ident <- ident/p) (token/p 'COLON) (type <- type/p) (pure (list ident type))))

(define rpc-call/p
  (do (token/p 'HASH)
      (fn <- (or/p field-access/p ident/p))
      (token/p 'LPAREN)
      (typed-args <- (many/p typed-field/p))
      (token/p 'RPAREN)
      (token/p 'HASH)
      (pure (list 'rpc-call fn (cons 'args typed-args)))))

(define expression/p (or/p rpc-call/p true/p false/p field-access/p ident/p))

(define hof/p (or/p (token/p 'MAP) (token/p 'FILTER) (token/p 'REDUCE)))

(define lambda/p
  (do (token/p 'LPAREN)
      [fn-args <- (many/p ident/p #:min 1 #:sep (token/p 'COMMA))]
      (token/p 'RPAREN)
      (token/p 'FAT-ARROW)
      [exprs <- expression/p]
      (pure `(lambda (fn-args . ,fn-args) ,exprs))))

(define map/p (do (token/p 'MAP) [callback <- lambda/p] (pure `(map ,callback))))

(define filter/p (do (token/p 'FILTER) [callback <- lambda/p] (pure `(filter ,@callback))))

(define functor/p (do (token/p 'PIPE) [f <- (or/p map/p filter/p lambda/p)] (token/p 'SEMI) (pure f)))

(define pipeline/p (do [applications <- (many/p functor/p #:min 1)] (pure applications)))

(define mfn/p
  (do (token/p 'MFN)
      [name <- ident/p]
      (token/p 'ASSIGNMENT)
      [input <- ident/p]
      [expression <- pipeline/p]
      (pure `(mfn (name . ,name) (input . ,input) (pipeline ,@expression)))))

(define sfn/p
  (do (token/p 'SFN)
      [name <- (token/p 'IDENTIFIER)]
      (token/p 'ASSIGNMENT)
      [input <- (token/p 'IDENTIFIER)]
      [pipeline <- pipeline/p]
      (pure (list 'mfn name input pipeline))))

(define module-def/p (or/p mfn/p sfn/p))

;; TODO Update this with an actual parser
;(define interface/p void/p)

(define streamline/p (do [cells <- (many/p (or/p module-def/p))] (pure cells)))

(define parser-result (parse-result! (parse-tokens streamline/p tokenized-input)))

(provide parser-result)
