#lang racket

(provide import-list
         instance-list)

;;; Global Params
(define import-list (make-parameter '()))
(define instance-list (make-parameter '()))
