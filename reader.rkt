#lang racket
;; #lang s-exp syntax/module-reader
;; streamline

;; #:whole-body-readers?
;; #t

;; #:read
;; streamline:read
;; #:read-syntax
;; streamline:read-syntax
;; #:info
;; get-info

(require megaparsack
         "lexer.rkt"
         "parser.rkt")

(define (streamline:read-syntax path)
  (define input (open-input-file path))
  (define ast (parse-streamline! input))
  ast)

(syntax->list (streamline:read-syntax "examples/simpleErc721.strm"))

(define (streamline:read in)
  42
  ;; (syntax->datum (tulip:read-syntax in))
  )

(define (get-info key default lookup-default)
  42
  ;; (case key
  ;;   [(color-lexer) lex-for-colorizer]
  ;;   [else (lookup-default key default)])
  )
