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

(define (pipeline? node)
  (eq? (car node) 'pipeline))

(define (lambda? node)
  (eq? (car node) 'lambda))

(define (map? node)
  (eq? (car node) 'map))

(define (fn-args? node)
  (eq? (car node) 'fn-args))

(define (generate-expression node)
  (match (car node)
    ['rpc-call (list (rust/g (cadr node)) "ARGS NOT DONE YET")]
    ['field-access (list (rust/g (cadr node)) (rust/g (caddr node)))]))

(define (generate-pipeline input-node)
  (map rust/g (cdr input-node)))

(define (generate-lambda input-node)
  (map rust/g (cdr input-node)))

(define (generate-map input-node)
  `(map ,(rust/g (car input-node))))

(define (generate-identifier node)
  (if (pair? (cdr node))
      (raise-argument-error "CDR OF THE IDENTIFIER NODE IS NOT A SYMBOL,IT'S A PAIR!")
      (symbol->string (cdr node))))

(define (generate-fn-args node)
  (map rust/g (cdr node)))

(define (generate-module node)
  ;; we are mapping over the caddr of the node
  (let* ([fn-parts (cdr node)]
         [name (cdr (assoc 'name fn-parts))]
         [inputs (cdr (assoc 'input fn-parts))]
         [pipeline (assoc 'pipeline fn-parts)])
    (map rust/g (list name inputs pipeline))))

(define (rust/g node)
  (cond
    [(identifier? node) (generate-identifier node)]

    [(fn-args? node) (generate-fn-args node)]

    [(expression? node) (generate-expression node)]

    [(map? node) (generate-map node)]

    [(pipeline? node) (generate-pipeline node)]

    [(lambda? node) (generate-lambda node)]

    [(module? node) (generate-module node)]

    [else (error (format "UNKNOWN NODE! ~a" node))]))

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
