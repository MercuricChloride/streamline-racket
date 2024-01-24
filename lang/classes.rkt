#lang racket

(require
 (prefix-in r: racket)
 "./globals.rkt"
 (for-syntax syntax/parse))

(provide (for-syntax (all-defined-out)))

(begin-for-syntax
  (define-syntax-class (source-def)
    #:literals (import-list)
    #:attributes (value code)
    (pattern #s(source-def path:expr)
      #:attr value #'path
      #:attr code #'(import-list (r:cons node.value (import-list)))
      ))

  ;; When we come across an instance-def,
  ;; we will just add the instance to the parameter tracking all instances
  (define-syntax-class (instance-def)
    #:literals (list import-list)
    #:attributes (value)
    (pattern #s(instance-def name:expr abi:expr addr:expr)
      #:attr value #'(list name abi addr)))

  (define-syntax-class (mfn-def)
    #:attributes (name* args* body*)
    (pattern #s(mfn name:expr (input:expr ...) body attributes)
      #:with name* (r:string->symbol (r:syntax->datum #'name))
      #:with body* #'42
      #:with args* (list 'a 'b 'c);; (syntax-local-introduce #'(r:define (name*) 42))
      ))
(define-syntax-class (fn-def)
    #:attributes (name* args* body*)
    (pattern #s(fn name:expr (input:expr ...) body attributes)
      #:with name* (r:string->symbol (r:syntax->datum #'name))
      #:with body* #'42
      #:with args* (list 'a 'b 'c);; (syntax-local-introduce #'(r:define (name*) 42))
      ))
  )
