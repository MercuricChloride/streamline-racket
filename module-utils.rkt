#lang racket

(require "./macros.rkt"
         "./parser.rkt")

(provide minput->name
         minput-w-type
         minput-map
         output-map-init
         get-store-type
         get-local-vars)

;; This module contains a bunch of helpers for working
;; with modules. Things like getting names from inputs etc.

;; Grabs the name from a module input edge
(define minput->name
  (match-lambda
    [(sfn-delta-edge from) from]
    [?
     string?
     ?]))

(define minput-w-type
  (match-lambda
    [(sfn-delta-edge from) (template "{{from}}: Deltas<DeltaProto<prost_wkt_types::Struct>>" from)]
    [?
     string?
     (template "{{?}}: prost_wkt_types::Struct" ?)]))

(define (minput-map edges callback)
  (as~> v edges (map callback v)))

(define (output-map-init inputs)
  (let ([inputs (minput-map inputs minput->name)])
    (if (= (length inputs) 1)
        (format "let mut output_map = ~a;" (first inputs))
        (format "let mut output_map = (~a);" (w-sep "," inputs)))))

(define (get-store-type attributes)
  (if (member "immutable" attributes)
      "StoreSetIfNotExistsProto<prost_wkt_types::Struct>"
      "StoreSetProto<prost_wkt_types::Struct>"))

;; Returns a list of `(name value) for each local variable defined in the attributes of a module
(define (get-local-vars attributes)
  (filter-map (match-lambda
                [(kv-attribute "var" name value) (list name value)]
                [_ false])
              attributes))
