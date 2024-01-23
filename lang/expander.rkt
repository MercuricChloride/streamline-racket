#lang racket

(require (prefix-in parser: streamline/lang/parser)
         (prefix-in r: racket)
         ;; We need to import the syntax-classes, from another file
         ;; at compile time, but the syntax classes are defined at run time
         (for-syntax syntax/parse))

;;; PARAMS FOR THE MODULE
(define import-list (make-parameter '()))
(define instance-list (make-parameter '()))

(define-syntax-rule (@%module-begin node ...) (r:#%module-begin node ...))

(define-syntax (streamline-datum stx)
  (syntax-parse stx
    [(_ . #s(source-def path:expr)) #'(import-list (r:cons path (import-list)))]
    [(_ . #s(instance-def name:expr abi:expr addr:expr))
     #'(instance-list (r:cons '(name abi addr) (instance-list)))]
    [(_ . #s(mfn name:expr (input:expr ...) body attributes))
     #:with sname (r:string->symbol (r:syntax->datum #'name))
     #:with inputs #'()
     ;; NOTE The use of syntax-local-introduce here allows us to define
     ;; our functions at the top level
     (syntax-local-introduce #'(r:define (sname) 42))]
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
         submod
         streamline:read-syntax
         streamline:read
         import-list
         require
         instance-list)

;;(streamline:read-syntax "asdf" (open-input-file "../examples/simpleErc721.strm"))
