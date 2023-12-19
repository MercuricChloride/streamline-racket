#lang racket

(require json)
(require dali)
(require "./parser.rkt")

(define rust-imports "use substreams::fuck::you;\n\n\n")

(define (fn-inputs input)
  (expand-string "{{#inputs}} {{ident}}: {{type}} {{/inputs}}" input))

(define (fn-template input)
  (expand-string
   "
   #[substreams::handlers::map]
   fn {{fn-name}}({{inputs}}) -> prost_wkt_types::Struct {
       {{expression}}
   }"
   input))

;; (define test-context
;;   (list "fn-name"
;;         "test_module"
;;         "inputs"
;;         (map fn-inputs (list (hash "idehnt" "a" "type" "address")))
;;         ","
;;         "expression"
;;         "6 + 9"))

;; (define test-output (fn-template test-context))
;; (pretty-display test-output)

;(expand-string "{{a}} {{b}}" (make-hash (list '("a" . "123") '("b" . "456"))))
;(pretty-display (include-template "templates/rust-helpers.rs"))

(define module-symbols '(mfn sfn))
(define expression-symbols '(boolean identifier field-access rpc-call))

(define (module? node)
  (memq (car node) module-symbols))

(define (expression? node)
  (memq (car node) expression-symbols))

(define (fn-body? node)
  (eq? (car node) 'expression))

(define (identifier? node)
  (eq? (car node) 'identifier))

(define (generate-expression node)
  (match (car node)
    ['identifier (symbol->string (cdr node))]
    ['rpc-call (map (lambda (node) (generate-expression (cdr node))) (cdr node))]
    ['field-access (map generate-expression (cdr node))]))

;; (define (generate-lambda node)
;;   (let* []))

(define (generate-identifier node)
  (if (pair? (cdr node))
      (raise-argument-error "CDR OF THE IDENTIFIER NODE IS NOT A SYMBOL,IT'S A PAIR!")
      (symbol->string (cdr node))))

(define (generate-module node)
  ;; we are mapping over the caddr of the node
  (let* ([fn-parts (cdr node)]
         [name (cdr (assoc 'name fn-parts))]
         [inputs (cdr (assoc 'input fn-parts))]
         [pipeline (cdr (assoc 'pipeline fn-parts))])
    (map rust/g (list name inputs pipeline))))

(define (rust/g node)
  (cond
    [(fn-body? node) (generate-expression (cdr node))]

    [(identifier? node) (generate-identifier node)]

    [(expression? node) (generate-expression node)]

    [(module? node) (generate-module node)]

    [else (raise (format "UNKNOWN NODE! ~a" node))]))

(define (compile ast)
  (for/fold ([acc rust-imports]) ([node ast])
    (let ([code (rust/g node)]) (values (string-append acc code)))))

;(generate-module (car parser-result))
; first module node
parser-result

; list of module inputs
;(cdadar parser-result)

; identifer of module
;(rust/g (cdadar parser-result))
;(cdddar parser-result)
(rust/g (car parser-result))
