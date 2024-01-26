#lang racket

(require (for-template streamline/utils/macros
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

;; [(_ . node:function-like)
;;  #:with name (string->symbol (syntax->datum #'node.name*))
;;  #:with (args ...) (map (lambda (arg) (string->symbol (syntax->datum arg)))
;;                         (syntax->list #'node.args*))
;;  #:with (functor ...) #'node.body*
;;  (syntax-local-introduce #'(define (name (~@ args ...))
;;                              (define input (list (~@ args ...)))
;;                              (~>> input (~@ functor ...))))]

;; Mfns, sfns, functions
(define-syntax-class (function-like)
  #:attributes (code)
  (pattern #s(mfn name*:string (input:ident ...) body:pipeline _)
    #:with name (systring->symbol #'name*)
    #:with (functor ...) #'body.code
    #:with (arg ...) #'(input.value ...)
    #:with code #'(define (name (~@ arg ...))
                    (define inputs '(arg ...))
                    (~>> inputs (~@ functor ...))))
  ;; (pattern #s(sfn name:string (input:string ...) body:pipeline _)
  ;;   #:with name* #'name
  ;;   #:with body* (syntax-e #'body.chain*)
  ;;   #:with args* #'(input ...))
  ;; (pattern #s(fn name:string (input:string ...) body:pipeline _)
  ;;   #:with name* #'name
  ;;   #:with body* (syntax-e #'body.chain*)
  ;;   #:with args* #'(input ...))
  )

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
  (pattern node:ident
    #:with code #'node.value))

(define-syntax-class (binary-op)
  #:attributes (code)
  (pattern #s(binary-op lh:expression op rh:expression)
    #:with op* (match (syntax-e #'op)
                 ["+" #'+]
                 ["-" #'-]
                 ["*" #'*]
                 ["/" #'/]
                 ["==" #'eq]
                 ["<" #'<]
                 [">" #'>])
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
