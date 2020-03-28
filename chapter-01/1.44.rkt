#lang racket

;; 1.44
(require "1.3.4-aux.rkt")
(require "1.43.rkt")
(require "1.1.7-aux.rkt")

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx)))
       3.0)))
(define n-smooth (lambda (n) (repeated smooth n)))
(((n-smooth 10) square) 10)

