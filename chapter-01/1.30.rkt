#lang racket

;; 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (id x) x)
(sum-iter id 1 inc 10)

