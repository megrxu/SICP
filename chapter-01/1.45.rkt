#lang racket

;; 1.45

(define (n-root n x)
  (define (power n)
    (lambda (x) (accumulate * 1 (lambda (y) x) 1 inc n)))
  (fixed-point ((repeated average-damp (- n 1))
                (lambda (y) (/ x ((power (- n 1)) y)))) 1.0))
(n-root 7 128)

