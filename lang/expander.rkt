#lang racket

(require (prefix-in parser: streamline/lang/parser)
         "./globals.rkt"
         streamline/utils/macros
         racket/stxparam
         (for-syntax "./classes.rkt" syntax/parse syntax/strip-context))

(define-syntax-rule (@%module-begin node ...) (#%module-begin node ...))

(define-syntax (streamline-datum stx)
  (syntax-parse stx
    [(_ . node:top-level-expression) #'node.code]
    [(_ . v) #'(#%datum . v)]))

(define (streamline:read-syntax path input)
  (define ast (parser:parse-streamline! input))
  ast)

(define (streamline:read in)
  (syntax->datum (streamline:read-syntax "" in)))

;; (define-syntax (@%app stx)
;;   (syntax-parse stx
;;     [(_ any ...) #'(#%app any ...)]))

(define-syntax (@%top-interaction stx)
  (syntax-parse stx
    #:datum-literals (sexp)
    [(_ sexp form:expr) #'form]
    [(_ sexp form:id) #'(#%top form)]))

(define-syntax-rule (@%top . id) (#%top . id))

(provide (rename-out (streamline-datum #%datum)
                     (@%module-begin #%module-begin)
                     (@%top-interaction #%top-interaction)
                     (@%top #%top))
         ;;(except-out (all-from-out racket) #%datum #%module-begin #%app #%top-interaction #%top)
         #%app
         streamline:read-syntax
         streamline:read
         import-list
         instance-list
         ~>>
         define
         begin-for-syntax)
;;(provide (for-syntax import-list instance-list))

;;(streamline:read-syntax "asdf" (open-input-file "../examples/simpleErc721.strm"))
