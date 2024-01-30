#lang racket/base

;;(for-template streamline/utils/macros streamline/lang/runtime-helpers)
(require streamline/lang/runtime-helpers
         racket/match
         (for-template "./globals.rkt")
         streamline/utils/macros
         (prefix-in parser: streamline/lang/parser)
         syntax/parse)

(provide transform)

;; (define-syntax (systring->symbol item)
;;   (string->symbol (syntax->datum item)))

(define (transform stx)
  (syntax-parse stx
    [node:top-level-expression #'node.code]))

(define-syntax-class (repl-interaction)
  #:attributes (code)
  (pattern node:expression
    #:attr code (syntax node.code))
  ;; (pattern node:source-def
  ;;   #:attr code (syntax-local-introduce #'node.code))
  ;; (pattern node:instance-def
  ;;   #:attr code (syntax-local-introduce #'node.code))
  ;; (pattern node:function-like
  ;;   #:attr code (syntax-local-introduce #'node.code))
  )

(define-syntax-class (top-level-expression)
  #:attributes (code)
  (pattern node:source-def
    #:attr code #'node.code)
  (pattern node:instance-def
    #:attr code #'node.code)
  (pattern node:function-like
    #:attr code #'node.code))

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
  #:literals (~>>)
  #:attributes (code)
  (pattern #s(mfn name*:ident (input:ident ...) body:pipeline (attribute:attribute ...))
    #:with name #'name*.value
    #:with (functor ...) #'body.code
    #:with (arg ...) #'(input.value ...)
    #:with (var ...) #'(attribute.code ...)
    #:with code #'(define (name arg ...)
                    (~@ var ...)
                    (define inputs (list arg ...))
                    (~>> inputs functor ...)))
  (pattern #s(sfn name*:ident (input:ident ...) body:pipeline (attribute:attribute ...))
    #:with name #'name*.value
    #:with (functor ...) #'body.code
    #:with (arg ...) #'(input.value ...)
    #:with (var ...) #'(attribute.code ...)
    #:with code #'(define (name arg ...)
                    (~@ var ...)
                    (define inputs (list arg ...))
                    (~>> inputs functor ...)))
  (pattern #s(fn name*:ident (input:ident ...) body:pipeline (attribute:attribute ...))
    #:with name #'name*.value
    #:with (functor ...) (syntax body.code)
    #:with (arg ...) (syntax (input.value ...))
    #:with (var ...) (syntax (attribute.code ...))
    #:with code #'(define (name arg ...)
                          (~@ var ...)
                          (define inputs (list arg ...))
                          (~>> inputs functor ...))))

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
    #:attr code (syntax (lambda (arg ...) body.code)))

  (pattern #s(hof hof-kind:ident #s(lam (arg*:ident ...) body:expression))
    #:with hof (syntax hof-kind.value)
    #:with (arg ...) (syntax (arg*.value ...))
    #:attr code (syntax (hof (lambda (arg ...) body.code)))))

(define-syntax-class (expression)
  #:attributes (code)
  (pattern node:primitive-type
    #:with code (syntax node.value))
  (pattern node:compound-type
    #:with code (syntax node.code))
  (pattern node:binary-op
    #:with code #'node.code)
  (pattern node:var-assignment
    #:with code #'node.code)

  (pattern node:field-access
    #:with code #'node.code)
  (pattern node:do-block
    #:with code #'node.code)
  (pattern node:function-call
    #:with code #'node.code)
  (pattern node:ident
    #:with code #'node.value))

(define-syntax-class (function-call)
  #:attributes (code)
  (pattern #s(function-call name:ident (arg:expression ...))
    #:attr code #'(name.value arg.code ...)))

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
    ;;#:do (print #'lh.code #'rh.code)
    #:with code #'(op* lh.code rh.code)))

(define-syntax-class (ident)
  #:attributes (value str-value)
  (pattern node:string
    #:with value (string->symbol (format "~a" (syntax->datum #'node)))
    #:with str-value (format "~a" (syntax->datum #'node))))

(define-syntax-class (field-access)
  #:attributes (code)
  (pattern #s(field-access map:ident (val:string ...))
    #:attr code #'(map-access map.value (list val ...))))

(define-syntax-class (key-value)
  #:attributes (key value)
  (pattern #s(key-value key*:ident value*:expression)
    #:attr key #'key*.str-value
    #:attr value #'value*.code))

(define-syntax-class (compound-type)
  #:attributes (code)
  (pattern #s(map-literal (kv:key-value ...))
    #:attr code #'(hash (~@ (~@ kv.key kv.value) ...)))
  (pattern #s(list-literal (val:expression ...))
    #:attr code (syntax (list val.code ...)))
  (pattern #s(tuple-literal (val:expression ...))
    #:attr code (syntax (list val.code ...))))

(define-syntax-class (primitive-type)
  #:attributes (value)
  (pattern #s(number-literal val:number)
    #:with value #'val)
  (pattern #s(string-literal val:string)
    #:with value #'val)
  (pattern #s(boolean-literal val:expr)
    #:with value #'val))
