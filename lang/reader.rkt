#lang racket

(require streamline/lang/classes
         (prefix-in parser: streamline/lang/parser)
         (for-syntax streamline/lang/configure-runtime
                     syntax/parse)
         streamline/lang/runtime-helpers
         syntax/strip-context)

(provide (rename-out (streamline:read-syntax read-syntax)
                     (streamline:read read))
                     ;(@%top-interaction #%top-interaction))
         (all-defined-out))

;; (define-syntax (@%top-interaction stx)
;;   (syntax-parse stx
;;     [(_ node ...) #'(streamline:read-interaction node ...)]
;;     ))

(define (streamline:read-syntax path input)
  (define ast (parser:parse-streamline! input))
  ;;(pretty-display (syntax->datum ast))
  (with-syntax ([path path]
                [(node ...) (map transform (syntax->list ast))])
    (strip-context #'(module asdf racket/base
                             (require streamline/utils/macros)
                             (require streamline/lang/runtime-helpers)
                             (require streamline/lang/globals)
                             (require streamline/lang/configure-runtime)
                             (configure-runtime!)
                             node ...))))

(define (streamline:read in)
  (streamline:read-syntax #f in))

