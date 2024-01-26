#lang racket

(provide add)

(define (->number val)
  (string->number (format "~a" val)))

(define (add lh rh)
  (cond
    [(string? lh) (format "~a~a" lh rh)]
    [(number? lh) (+ lh (->number rh))]))
