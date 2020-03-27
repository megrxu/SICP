#lang racket

;; 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(accumulate + 0 id 1 inc 100) ;; 5050
(accumulate * 1 id 1 inc 5) ;; 120

