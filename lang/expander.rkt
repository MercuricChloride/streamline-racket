#lang racket

(require streamline/lang/reader
         syntax/parse
         syntax/strip-context)

(define (streamline-begin stx)
  (syntax-parse stx
    #:context '|error while parsing module|
    [(_ _) (strip-context #'(#%module-begin "hi"))]))
