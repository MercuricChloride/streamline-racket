#lang racket

(require (prefix-in parser: streamline/lang/parser)
         (prefix-in r: racket)
         ;; We need to import the syntax-classes, from another file
         ;; at compile time, but the syntax classes are defined at run time
         (for-syntax syntax/parse))

;;; PARAMS FOR THE MODULE
(define import-list (make-parameter '()))

(define-syntax-rule (@%module-begin node ...) (r:#%module-begin node ...))

(define-syntax (streamline-datum stx)
  (syntax-parse stx
    [(_ . #s(source-def path:expr)) #'(import-list (r:cons path (import-list)))]
    [(_ . v)
     #'(begin
         (r:print "hi")
         (r:#%datum v))]))

(define (streamline:read-syntax path input)
  (define ast (syntax->list (parser:parse-streamline! input)))
  ast)

(define (streamline:read in)
  (syntax->datum (streamline:read-syntax "" in)))

(define-syntax (@%app stx)
  (syntax-parse stx
    [(_ import-list) #'(import-list)]))

(provide (rename-out (streamline-datum #%datum) (@%module-begin #%module-begin) (@%app #%app))
         #%top
         #%top-interaction
         streamline:read-syntax
         streamline:read
         import-list)

;;(streamline:read-syntax "asdf" (open-input-file "../examples/simpleErc721.strm"))
