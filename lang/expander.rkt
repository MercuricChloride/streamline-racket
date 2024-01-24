#lang racket

(require (prefix-in parser: streamline/lang/parser)
         (prefix-in r: racket)
         ;; We need to import the syntax-classes, from another file
         ;; at compile time, but the syntax classes are defined at run time
         ;; Or I guess we can also just defined the syntax-classes at phase 1
         ;; and import at runtime ie phase 0?
         "./classes.rkt"
         "./globals.rkt"
         (for-syntax syntax/parse "./globals.rkt"))

(provide (for-syntax import-list instance-list))

(define-syntax-rule (@%module-begin node ...) (r:#%module-begin node ...))

(define-syntax (streamline-datum stx)
  (syntax-parse stx
    ;; Whenever a source def is declared, we add it to the import-list global var
    [(_ . node:source-def) #'(import-list (r:cons node.value (import-list)))]
    ;; Whenever a constant instance is declared, we add it to the instance-list global var
    [(_ . node:instance-def) #'(instance-list (r:cons node.value (instance-list)))]
    ;; Whenever an mfn is declared, we will define a new top level function
    [(_ . node:mfn-def) (syntax-local-introduce #'(r:define (node.name*) node.body*))]
    [(_ . node:fn-def) (syntax-local-introduce #'(r:define (node.name*) node.body*))]
    [(_ . v) #'(#%datum v)]))

(define (streamline:read-syntax path input)
  (define ast (parser:parse-streamline! input))
  ast)

(define (streamline:read in)
  (syntax->datum (streamline:read-syntax "" in)))

(define-syntax (@%app stx)
  (syntax-parse stx
    #:datum-literals (import-list)
    [(_ import-list) #'(import-list)]
    [(_ any ...) #'(#%app any ...)]))

(define-syntax (@%top-interaction stx)
  (syntax-parse stx
    #:datum-literals (sexp)
    [(_ sexp form:expr) #'form]
    [(_ sexp form:id) #'(@%top form)]))

(provide (rename-out (streamline-datum #%datum)
                     (@%module-begin #%module-begin)
                     (@%app #%app)
                     (@%top-interaction #%top-interaction))
         #%provide
         #%top
         define
         streamline:read-syntax
         streamline:read
         import-list
         instance-list
         require)

;;(streamline:read-syntax "asdf" (open-input-file "../examples/simpleErc721.strm"))
