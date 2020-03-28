#lang racket

;; 1.6

(require "1.1.7-aux.rkt")

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x) x)))

(new-sqrt-iter 1.0 1) ;; halt

