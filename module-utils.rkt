#lang racket

(require "./macros.rkt")
(require "./parser.rkt")

(provide minput->name
         minput-map)

;; This module contains a bunch of helpers for working
;; with modules. Things like getting names from inputs etc.

;; Grabs the name from a module input edge
(define minput->name
  (match-lambda
    [(sfn-delta-edge from) from]
    [?
     string?
     ?]))

(define (minput-map edges callback)
  (~>> edges (map callback)))
