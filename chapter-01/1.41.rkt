#lang racket

;; 1.41
(require "1.29-aux.rkt")

(define (double f)
  (lambda (x) (f (f x))))
((double inc) 2)

(provide double)