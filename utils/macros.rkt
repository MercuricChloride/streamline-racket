#lang racket
(require dali
         (for-syntax syntax/parse))

(provide ihash
         template
         def-template
         thread-replace
         ~>>
         as~>
         w-sep)
;; w-local-vars

;; Converts a series of idents into a hashmap from (string of the ident) -> ident value
;; We use this for templating
(define-syntax (ihash stx)
  (syntax-parse stx
    [(_ key ...) #'(hash (~@ (symbol->string 'key) key) ...)]))

;; Allows for shorthand string templating, where we construct a hashmap with the names of the idents passed in
(define-syntax (template stx)
  (syntax-parse stx
    [(_ str key ...) #'(expand-string str (ihash key ...))]))

;; Allows for the creation of a template
(define-syntax (def-template stx)
  (syntax-parse stx
    [(_ (name param ...) str:expr)
     #'(define (name (~@ param) ...)
         (template str (~@ param) ...))]
    [(_ (name param ...) ((~seq namev:id remap:expr) ...) str:expr)
     #'(define (name (~@ param) ...)
         (let* ([~? (namev remap)] ...) (template str (~@ param) ... (~@ namev) ...)))]))

;; Thread Replace
;; Allows us to easily define replacements
;; Note that our particular use of it adds the replacement at the end of the replacement,
;; because we are using this to update text, not replace it
(define-syntax (thread-replace stx)
  (syntax-parse stx
    [(_ str:expr (~seq find:expr replace:expr) ...)
     #'(let* ([output str]
              [output (string-replace output find (format "~a\n~a" replace find) #:all? true)] ...)
         output)]))

(begin-for-syntax
  (define-syntax-class (callback)
    #:attributes (input* procedure*)
    (pattern (value:expr proc:expr callback:expr)
      #:with procedure* #'(proc callback value)
      #:with input* #'value)))

;; clojure style thread-last macros
(define-syntax (~>> stx)
  (syntax-parse stx
    #:literals (lambda)
    [(_ value:expr (~or (lambda (arg:id ...) body) (hof-proc:expr hof-callback:expr)) ...)
     #'(let* ([val value]
              [~?
               (~@ (val ((lambda (arg ...) body) val)) ...)
               (~@ (val (hof-proc hof-callback val)) ...)])
         val)]))

(~>> 42 (lambda (arg) (* 2 arg)))
(~>> '(1 2 3) (map (lambda (num) (* 2 num))))

;; clojure style threader macros
(define-syntax (as~> stx)
  (syntax-parse stx
    [(_ ident:id initial-value:expr expression:expr ...)
     #'(let* ([ident initial-value] [~@ (ident expression)] ...) ident)]))

;; a small wrapper over string join.
;; Will take the last expression given and string join it with the seperator
(define-syntax (w-sep stx)
  (syntax-parse stx
    [(_ sep:expr expression:expr ...+)
     #'(string-join (begin
                      expression ...)
                    sep)]
    [(_ sep:expr) #'""]))
