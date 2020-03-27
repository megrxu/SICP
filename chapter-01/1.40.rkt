#lang racket

;; 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))
(newton-method (cubic 1 -2 -30) 1)

