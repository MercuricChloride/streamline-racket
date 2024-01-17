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
    [(_ (name param ...) (namev:id remap:expr ...) str:expr)
     #'(define (name (~@ param) ...)
         (let ([~? (namev remap)] ...) (template str (~@ param) ...)))]))

;; Thread Replace
;; Allows us to easily define replacements
(define-syntax (thread-replace stx)
  (syntax-parse stx
    [(_ str:expr (~seq find:expr replace:expr) ...)
     #'(let* ([output str]
              [output (string-replace output find (format "~a\n~a" replace find) #:all? true)] ...)
         output)]))

(define-syntax (~>> stx)
  (syntax-parse stx
    [(_ value:expr (proc:expr callback:expr) ...)
     #'(let* ([val value] [~@ (val (proc callback val))] ...) val)]))

(define-syntax (as~> stx)
  (syntax-parse stx
    [(_ ident:id initial-value:expr expression:expr ...)
     #'(let* ([ident initial-value] [~@ (ident expression)] ...) ident)]))

;; a small wrapper over string join.
;; Will take the last expression given and string join it with the seperator
(define-syntax (w-sep stx)
  (syntax-parse stx
    [(_ sep:expr expression:expr ...)
     #'(string-join (begin
                      expression ...)
                    sep)]))

;;(~>> '(1 2 3 4) (map (λ (num) (* num 2))) (map (λ (num) (* num 2))) (filter (λ (num) (< 10 num))))

;;(as~> t '(1 2 3 4) (map (λ (num) (* num 2)) t) (format "Hello from the format-string! ~a" t))
