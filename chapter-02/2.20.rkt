#lang racket

;; 2.20

(define (same-parity x . y)
  (filter (lambda (i) (even? (- i x))) y))

(same-parity 1 2 3 4 5 6 7 8 9)
(same-parity 1 -3 -9 1 0 3 7)

