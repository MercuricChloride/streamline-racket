#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/functor
         data/applicative
         "./lexer.rkt")
;; EXPRESSIONS

(define true/p (do (token/p 'TRUE) (pure `(boolean ,#t))))
(define false/p (do (token/p 'FALSE) (pure `(boolean ,#f))))
(define string-literal/p (do (str <- (token/p 'STRING)) (pure `(string-literal ,str))))
(define number-literal/p (do (num <- (token/p 'NUMBER)) (pure `(number-literal ,num))))

(define ident/p (do [ident <- (token/p 'IDENTIFIER)] (pure `(identifier ,ident))))
(define type/p (do (ident <- ident/p) (pure `(type ,ident))))

(define field-access/p
  (do [lh <- ident/p]
      (token/p 'DOT)
      (rh <- (or/p (try/p field-access/p) ident/p))
      (pure `(field-access ,lh ,rh))))

(define key-value/p
  (do [key <- ident/p] (token/p 'COLON) [val <- (lazy/p expression/p)] (pure `(key-value ,key ,val))))

(define map-literal/p
  (do (token/p 'LCURLY)
      [kvs <- (many/p key-value/p #:min 1 #:sep (token/p 'COMMA))]
      (token/p 'RCURLY)
      (pure `(map-literal ,kvs))))

(define typed-field/p
  (do (ident <- ident/p) (token/p 'COLON) (type <- type/p) (pure (list ident type))))

(define rpc-call/p
  (do (token/p 'HASH)
      (fn <- (or/p field-access/p ident/p))
      (token/p 'LPAREN)
      (typed-args <- (many/p typed-field/p))
      (token/p 'RPAREN)
      (token/p 'HASH)
      (pure `(rpc-call ,fn (args ,typed-args)))))

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
      (pure `(lambda (fn-args ,fn-args) ,exprs))))

(define map/p (do (token/p 'MAP) [callback <- lambda/p] (pure `(map ,callback))))

(define filter/p (do (token/p 'FILTER) [callback <- lambda/p] (pure `(filter ,callback))))

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
      (pure `(mfn ,name ,input ,pipeline))))

(define module-def/p (or/p mfn/p sfn/p))

(define instance-def/p
  (do (name <- ident/p)
      (token/p 'ASSIGNMENT)
      (abi-type <- ident/p)
      (token/p 'LPAREN)
      (address <- (token/p 'ADDRESS))
      (token/p 'RPAREN)
      (token/p 'SEMI)
      (pure `(instance ,name ,abi-type ,address))))

(define streamline/p (do [cells <- (many/p (or/p module-def/p instance-def/p))] (pure cells)))

(define parser-result (parse-result! (parse-tokens streamline/p tokenized-input)))
parser-result

(provide parser-result)
