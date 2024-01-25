#lang racket

(require (for-template "./globals.rkt" racket/base)
         syntax/parse)

(provide source-def instance-def function-like)

(define-syntax-rule (syntax->symbol item) #'(string->symbol (syntax->datum item)))

(define-syntax-class (source-def)
  #:attributes (value code)
  (pattern #s(source-def path:expr)
    #:attr value #'path
    #:attr code #'(import-list (r:cons node.value (import-list)))))

;; When we come across an instance-def,
;; we will just add the instance to the parameter tracking all instances
(define-syntax-class (instance-def)
  #:attributes (value)
  (pattern #s(instance-def name:expr abi:expr addr:expr)
    #:attr value #'(list name abi addr)))

;; Mfns, sfns, functions
(define-syntax-class (function-like)
  #:attributes (name* args* body*)
  (pattern #s(mfn name:expr (input ...) body attributes)
    #:with name* #'name
    #:with body* 42
    #:with args* #'(input ...))
  (pattern #s(sfn name:expr (input ...) body attributes)
    #:with name* #'name
    #:with body* 42
    #:with args* #'(input ...))
  (pattern #s(fn name:expr (input ...) body attributes)
    #:with name* #'name
    #:with body* 42
    #:with args* #'(input ...)))
