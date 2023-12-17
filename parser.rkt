#lang racket

(require megaparsack megaparsack/parser-tools/lex
         data/monad
         data/functor
         "./lexer.rkt")

(define module-kind/p
  (or/p (token/p 'MFN)
        (token/p 'SFN)))

(define hof/p
  (or/p (token/p 'MAP)
        (token/p 'FILTER)
        (token/p 'REDUCE)))

(define lambda/p
  (do (token/p 'LPAREN)
    (token/p 'IDENTIFIER)
    (token/p 'RPAREN)
    (token/p 'FAT-ARROW)
    (token/p 'EXPRESSION)))

(define functor/p
  (do (token/p 'PIPE)
    hof/p
    lambda/p))

(define pipeline/p
  (many/p
   functor/p
   #:min 1))

(define module-def/p
  (do module-kind/p
    (token/p 'IDENTIFIER)
    (token/p 'ASSIGNMENT)
    (token/p 'IDENTIFIER)
    pipeline/p))

(parse-result! (parse-tokens module-def/p tokenized-input))