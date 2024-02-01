#lang racket

(require (prefix-in parser: streamline/lang/parser)
         (prefix-in r: racket/base)
         streamline/utils/macros
         streamline/lang/runtime-helpers
         syntax/strip-context
         "./classes.rkt"
         (for-syntax "./classes.rkt" syntax/parse))

(define-syntax-rule (@%module-begin node ...) (#%module-begin (provide (all-defined-out)) node ...))

;; This determines whether or not we are in the repl or parsing a top level file
(define interactive? (make-parameter #f))
(define current-block (make-parameter 123))
(define block-data
  (make-parameter
   (hash 123
         (hash "number" "Hey this is a block number" "hash" "this is a hash")
         124
         (hash "other number" "Hey this is another block number" "hash" "this is another hash"))))

(define (get-block-data field)
  (map-access (block-data) (list (current-block) field)))

;; (define-syntax (streamline-datum stx)
;;   (syntax-parse stx
;;     [(_ . node:top-level-expression) (syntax node.code)]
;;     [(_ node:repl-interaction) (syntax node.code)]
;;     [(_ . v) (syntax (#%datum . v))]))

;; This is because repl interaction hash different than the rules for streamline files,
;; So we just need to define some additional "top-level-expressions" which are allowed.
;; (define (streamline-repl-interaction stx)
;;   (syntax-parse stx
;;     [(_ node:repl-interaction) (syntax node.code)]))

;; (define (streamline:read-interaction input)
;;   (define ast (parser:parse-streamline-interaction! input))
;;   (streamline-repl-interaction ast))

;;(define-syntax-rule (@%top . id) (#%top . id))

(provide ;(rename-out ;(streamline-datum #%datum)
                     ;(@%module-begin #%module-begin)
                     ;(@%top #%top)
                     ;(streamline:read-syntax read-syntax)
                     ;(streamline:read read))
         block-data
         (all-from-out streamline/lang/runtime-helpers)
         get-block-data
         current-block
         ~>>)

(define (configure-runtime!)
  (current-read-interaction
   (λ (src in)
     ; The REPL works pretty differently when used at the terminal and when used from within DrRacket.
     ; Therefore, it’s necessary to branch on the result of terminal-port? so we can check what
     ; behavior to expect.
     ;
     ; Additionally, when readline is loaded, it installs itself in place of the terminal port with
     ; the name 'readline-input. Therefore, we should handle that case the same way.
     ;; (if (and (char-ready? in) (not (eof-object? (peek-char in))))
     ;;     (streamline:read-interaction in)
     ;;     eof)
     (if (or (terminal-port? in) (eq? (object-name in) 'readline-input))
         ; At the terminal, input is delimited by newlines. Therefore, we should read a line at a time
         ; before handing things off to the lexer and parser. If we ever get #<eof>, we should pass it
         ; through. That way, the user can exit the REPL by sending ^D.
         (let ([line (read-line in)])
           (if (eof-object? line)
               eof
               (datum->syntax (syntax->datum (streamline:read-interaction in)))))
         ; In DrRacket, multi-line input is completely possible, so #<eof>s are inserted between each
         ; interaction within the port. Therefore, we should just lex/parse the whole thing. We need
         ; to actually return #<eof> in order for the REPL to advance to the next prompt, though (for
         ; whatever reason), so we’ll also pass lone #<eof>s through here.
         (if (and (char-ready? in) (not (eof-object? (peek-char in))))
             (datum->syntax #f (syntax->datum (streamline:read-interaction in)))
             eof))
     )))
