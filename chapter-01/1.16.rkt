#lang racket

;; 1.16

(define (expt b n)
  (define (iter a b n)
    (cond ((= n 0) 1)
          ((= n 1) (* a b))
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) (square b) (quotient n 2)))))
  (iter 1 b n))

(expt 5 3)
(expt 2 8)

