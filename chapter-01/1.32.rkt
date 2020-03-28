#lang racket

;; 1.32
(require "1.30.rkt" )
(require "1.29-aux.rkt" )

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(accumulate + 0 id 1 inc 100) ;; 5050
(accumulate * 1 id 1 inc 5) ;; 120

(provide accumulate id inc)