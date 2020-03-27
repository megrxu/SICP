#lang racket

;; 1.41

(define (double f)
  (lambda (x) (f (f x))))
((double inc) 2)

