#lang racket

(require (for-template "./globals.rkt" racket/base)
         syntax/parse)

(provide source-def
         instance-def
         function-like
         primitive-type
         pipeline
         functor)

(define-syntax-rule (syntax->symbol item) #'(string->symbol (syntax->datum item)))

(define-syntax-class (source-def)
  #:attributes (value code)
  (pattern #s(source-def path:expr)
    #:attr value #'path
    #:attr code #'(import-list (cons value (import-list)))))

;; When we come across an instance-def,
;; we will just add the instance to the parameter tracking all instances
(define-syntax-class (instance-def)
  #:attributes (value)
  (pattern #s(instance-def name:expr abi:expr addr:expr)
    #:attr value #'(list name abi addr)))

(define-syntax-class (pipeline)
  #:attributes (chain*)
  (pattern #s(pipeline (functors:functor ...))
    #:with chain* #'(functors.code* ...)))

;; Mfns, sfns, functions
(define-syntax-class (function-like)
  #:attributes (name* args* body*)
  (pattern #s(mfn name:string (input:string ...) body:pipeline _)
    #:with name* #'name
    #:with body* (first (syntax-e #'body.chain*))
    #:with args* #'(input ...))
  (pattern #s(sfn name:string (input:string ...) body:pipeline _)
    #:with name* #'name
    #:with body* #'body.chain*
    #:with args* #'(input ...))
  (pattern #s(fn name:string (input:string ...) body:pipeline _)
    #:with name* #'name
    #:with body* #'body.chain*
    #:with args* #'(input ...)))

(define-syntax-class (functor)
  #:attributes (args* body* code*)
  (pattern #s(lam (arg ...) body:expression)
    #:with (arg* ...) (map (lambda (arg) (string->symbol (syntax->datum arg)))
                           (syntax->list #'(arg ...)))
    #:with args* #'(arg* ...)
    #:with body* #'body
    #:attr code* #'(lambda (arg* ...) body.code*)))

(define-syntax-class (expression)
  #:attributes (code*)
  (pattern node:primitive-type
      #:with code* #'node.value*))

(define-syntax-class (primitive-type)
  #:attributes (value*)
  (pattern #s(number-literal val)
    #:with value* #'val)
  (pattern #s(string-literal val)
    #:with value* #'val)
  (pattern #s(boolean-literal val:expr)
    #:with value* #'val)
  ;; TODO Maybe add a sequence type?
  (pattern #s(list-literal val:expr)
    #:with value* #'val)
  (pattern #s(tuple-literal val:expr)
    #:with value* #'val))
