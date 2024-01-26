#lang racket

(provide (all-defined-out))

(define (->number val)
  (string->number (format "~a" val)))

(define (add lh rh)
  (cond
    [(string? lh) (format "~a~a" lh rh)]
    [(number? lh) (+ lh (->number rh))]
    [(box? lh) (add (unbox lh) rh)]))

(define (maybe-get ht key)
  (cond
    [(box? ht) (maybe-get (unbox ht) key)]
    [(not (hash? ht))
     (begin
       (pretty-print
        (format "Invalid Key Access for non-map value. Tried to grab key: \"~a\" on value: \"~a\""
                key
                ht))
       null)]
    [(hash-has-key? ht key) (hash-ref ht key)]
    [else
     (begin
       (pretty-print (format "Invalid Key Access for key: \"~a\" in hash table: \"~a\"" key ht))
       null)]))

(define (map-access ht keys)
  (cond
    [(null? ht) null]
    [(= 1 (length keys)) (maybe-get ht (car keys))]
    [else (map-access (maybe-get ht (car keys)) (cdr keys))]))
