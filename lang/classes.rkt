#lang racket

(require (for-template streamline/utils/macros
                       streamline/lang/runtime-helpers
                       "./globals.rkt"
                       racket/base
                       racket/stxparam
                       racket/stxparam-exptime)
         syntax/parse
         racket/stxparam)

(provide source-def
         instance-def
         function-like
         primitive-type
         pipeline
         functor
         expression
         top-level-expression
         systring->symbol)

;; (define-syntax-parameter string->ident? #f)

;; (define-syntax-rule (with-ident-string body ...)
;;   (syntax-parameterize ([string->ident? #t])
;;     body ...))

(define-syntax-rule (systring->symbol item) (string->symbol (syntax->datum item)))

(define-syntax-class (top-level-expression)
  #:attributes (code)
  (pattern node:source-def
    #:attr code (syntax-local-introduce #'node.code))
  (pattern node:instance-def
    #:attr code (syntax-local-introduce #'node.code))
  (pattern node:function-like
    #:attr code (syntax-local-introduce #'node.code)))

(define-syntax-class (source-def)
  #:attributes (value code)
  (pattern #s(source-def path)
    #:attr value #'path
    #:attr code #'(import-list (cons (string->symbol value) (import-list)))))

;; When we come across an instance-def,
;; we will just add the instance to the parameter tracking all instances
(define-syntax-class (instance-def)
  #:attributes (code)
  (pattern #s(instance-def name:expr abi:expr addr:expr)
    #:attr code #'(list name abi addr)))

;; Mfns, sfns, functions
(define-syntax-class (function-like)
  #:attributes (code)
  (pattern #s(mfn name*:string (input:ident ...) body:pipeline (attribute:attribute ...))
    #:with name (systring->symbol #'name*)
    #:with (functor ...) #'body.code
    #:with (arg ...) #'(input.value ...)
    #:with (var ...) #'(attribute.code ...)
    #:with code #'(define (name (~@ arg ...))
                    (~@ var ...)
                    (define inputs (list (~@ arg ...)))
                    (~>> inputs (~@ functor ...))))
  (pattern #s(sfn name*:string (input:ident ...) body:pipeline (attribute:attribute ...))
    #:with name (systring->symbol #'name*)
    #:with (functor ...) #'body.code
    #:with (arg ...) #'(input.value ...)
    #:with (var ...) #'(attribute.code ...)
    #:with code #'(define (name (~@ arg ...))
                    (~@ var ...)
                    (define inputs (list (~@ arg ...)))
                    (~>> inputs (~@ functor ...))))
  (pattern #s(fn name*:string (input:ident ...) body:pipeline (attribute:attribute ...))
    #:with name (systring->symbol #'name*)
    #:with (functor ...) #'body.code
    #:with (arg ...) #'(input.value ...)
    #:with (var ...) #'(attribute.code ...)
    #:with code #'(define (name (~@ arg ...))
                    (~@ var ...)
                    (define inputs (list (~@ arg ...)))
                    (~>> inputs (~@ functor ...)))))

(define-syntax-class (attribute)
  #:attributes (code)
  (pattern #s(kv-attribute "var" key:ident value:expression)
    #:with code #'(define key.value (box value.code))))

(define-syntax-class (pipeline)
  #:attributes (code)
  (pattern #s(pipeline (functors:functor ...))
    #:with code #'(functors.code ...)))

(define-syntax-class (functor)
  #:attributes (code)
  (pattern #s(lam (arg*:ident ...) body:expression)
    #:with (arg ...) #'(arg*.value ...)
    #:attr code #'(lambda (arg ...) body.code)))

(define-syntax-class (expression)
  #:attributes (code)
  (pattern node:primitive-type
    #:with code #'node.value*)
  (pattern node:binary-op
    #:with code #'node.code)
  (pattern node:var-assignment
    #:with code #'node.code)
  (pattern node:do-block
    #:with code #'node.code)
  (pattern node:ident
    #:with code #'node.value))

(define-syntax-class (var-assignment)
  #:attributes (code)
  (pattern #s(var-assignment key:ident value:expression)
    #:attr code #'(set-box! key.value value.code)))

(define-syntax-class (do-block)
  #:attributes (code)
  (pattern #s(do-block (expr:expression ...))
    #:attr code #'(begin
                    expr.code ...)))

(define-syntax-class (binary-op)
  #:attributes (code)
  (pattern #s(binary-op lh:expression op rh:expression)
    #:with op* (match (syntax-e #'op)
                 ["+" #'add]
                 ["-" #'-]
                 ["*" #'*]
                 ["/" #'/]
                 ["==" #'eq]
                 ["<" #'<]
                 [">" #'>])
    #:do (print #'lh.code #'rh.code)
    #:with code #'(op* lh.code rh.code)))

(define-syntax-class (ident)
  #:attributes (value)
  (pattern node:string
    #:with value (string->symbol (format "~a" (syntax->datum #'node)))))

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
