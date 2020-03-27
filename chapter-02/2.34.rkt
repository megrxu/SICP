#lang racket

;; 2.34

(define (horner-eval x coeffs)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coeffs))

(horner-eval 2 (list 1 3 0 5 0 1))

