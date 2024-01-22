#lang s-exp syntax/module-reader
racket

#:whole-body-readers?
#t

#:read
streamline:read
#:read-syntax
streamline:read-syntax

(require megaparsack
         streamline/lang/parser
         streamline/lang/lexer)

(define (streamline:read-syntax path input)
  (define ast (parse-streamline! input))
  ast)

(define (streamline:read in)
  (syntax->datum (streamline:read-syntax in)))
