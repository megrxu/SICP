#lang racket

;; 1.17-18

(define (fast-* a b)
  (define (double x) (+ x x))
  (define (iter r a b)
    (cond ((= b 0) 0)
          ((= b 1) (+ r a))
          ((even? b) (iter r (double a) (/ b 2)))
          (else (iter (+ r a) (double a) (quotient b 2)))))
  (iter 0 a b))

(fast-* 10 12)
(= (fast-* 1123 123) (* 1123 123))

