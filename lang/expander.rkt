#lang racket

(require (prefix-in parser: streamline/lang/parser)
         "./globals.rkt"
         streamline/utils/macros
         (for-syntax "./classes.rkt" syntax/parse syntax/strip-context))

(define-syntax-rule (@%module-begin node ...) (#%module-begin node ...))

(define-syntax (streamline-datum stx)
  (syntax-parse stx
    ;; Whenever a source def is declared, we add it to the import-list global var
    [(_ . node:source-def) #'node.code]
    ;; Whenever a constant instance is declared, we add it to the instance-list global var
    [(_ . node:instance-def) #'(instance-list (cons node.value (instance-list)))]
    ;; Whenever an mfn is declared, we will define a new top level function
    [(_ . node:function-like)
     #:with name (string->symbol (syntax->datum #'node.name*))
     #:with (args ...) (map (lambda (arg) (string->symbol (syntax->datum arg)))
                            (syntax->list #'node.args*))
     #:with body #'node.body*
     (syntax-local-introduce #'(define (name (~@ args ...))
                                 (define input (list (~@ args ...)))
                                 (~>> input body)))]
    [(_ . node:primitive-type) #'node.value*]
    ;; [(_ . node:pipeline) #'node.chain*]
    ;; [(_ . node:lam)
    ;;  #:with (arg ...) (map (lambda (arg) (string->symbol (syntax->datum arg)))
    ;;                        (syntax->list #'node.args*))
    ;;  #:with body #'node.body*
    ;;  #'(lambda (arg ...) body)]
    [(_ . v) #'(#%datum . v)]))

(define (streamline:read-syntax path input)
  (define ast (parser:parse-streamline! input))
  ast)

(define (streamline:read in)
  (syntax->datum (streamline:read-syntax "" in)))

(define-syntax (@%app stx)
  (syntax-parse stx
    [(_ any ...) #'(#%app any ...)]))

(define-syntax (@%top-interaction stx)
  (syntax-parse stx
    #:datum-literals (sexp)
    [(_ sexp form:expr) #'form]
    [(_ sexp form:id) #'(#%top form)]))

(define-syntax-rule (@%top . id) (#%top . id))

(provide (rename-out (streamline-datum #%datum)
                     (@%module-begin #%module-begin)
                     (@%app #%app)
                     (@%top-interaction #%top-interaction)
                     (@%top #%top))
         streamline:read-syntax
         streamline:read
         import-list
         instance-list
         ~>>
         define
         begin-for-syntax)
;;(provide (for-syntax import-list instance-list))

;;(streamline:read-syntax "asdf" (open-input-file "../examples/simpleErc721.strm"))
