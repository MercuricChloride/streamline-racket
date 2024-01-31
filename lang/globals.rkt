#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/pretty)

(provide import-list
         instance-list
         module-dependencies)

;;; Global Params
(define import-list (make-parameter '()))
(define instance-list (make-parameter '()))
; A map from module-name -> it's input modules, or an empty list if none
(define module-dependencies (make-parameter (make-hash '(("BLOCK" . ())))))
(define modules (make-parameter (make-hash)))
(define block-value (make-parameter '()))

(define-syntax (defmodule stx)
  (syntax-parse stx
    [(_ (name:id args:id ...) body ...)
     #'(begin
         (hash-set! (module-dependencies) (symbol->string 'name) (list (symbol->string 'args) ...))
         (hash-set! (modules)
                    (symbol->string 'name)
                    (lambda (args ...)
                      body ...)))]))

(define (eval-module name inputs)
  (apply (hash-ref (modules) name) inputs))

(define (eval-input input)
  (if (equal? input "BLOCK") (block-value) (eval-block input)))

(define (eval-block module-name)
  (define input-modules (hash-ref (module-dependencies) module-name))
  (define input-values (map eval-input input-modules))
  (eval-module module-name input-values))

(define (exec-block block-data module-name)
  (parameterize ([block-value block-data])
    (eval-block module-name)))

(defmodule (bar BLOCK) (* 10 (hash-ref BLOCK "number")))
(defmodule (baz BLOCK) (* 10 (hash-ref BLOCK "timestamp")))
(defmodule (zap BLOCK) (* 10 (hash-ref BLOCK "timestamp")))
(defmodule (foo bar baz zap) (hash "bar" bar "baz" baz "zap" zap))

(parameterize ([block-value (hash "number" 420 "timestamp" 69)])
  (eval-block "foo"))
