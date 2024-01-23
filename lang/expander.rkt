#lang racket

(require (prefix-in parser: streamline/lang/parser)
         ;; We need to import the syntax-classes, from another file
         ;; at compile time, but the syntax classes are defined at run time
         (for-syntax syntax/parse "./classes.rkt"))

;;; PARAMS FOR THE MODULE
(define import-list (make-parameter '()))

(define-syntax-rule (@%module-begin node ...) (#%module-begin node ...))

(define-syntax (streamline-datum stx)
  (syntax-parse stx
    [(_ . #s(source-def path:expr)) #'(import-list (cons path (import-list)))]
    [(_ . v) #'(#%datum v)]))

(define (streamline:read-syntax path input)
  (define ast (parser:parse-streamline! input))
  #`(module streamline-module streamline
      #,ast))

(define (streamline:read in)
  (syntax->datum (streamline:read-syntax "" in)))

(define-syntax (@%app stx)
  (syntax-parse stx
    [(_ import-list) #'(import-list)]))

(provide (rename-out (streamline:read read)
                     (streamline:read-syntax read-syntax)
                     (streamline-datum #%datum)
                     (@%module-begin #%module-begin)
                     (@%app #%app))
         #%top
         #%top-interaction
         provide
         import-list)
