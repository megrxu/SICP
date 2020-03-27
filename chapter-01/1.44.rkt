#lang racket

;; 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx)))
       3.0)))
(define n-smooth (lambda (n) (repeated smooth n)))
(((n-smooth 10) square) 10)

