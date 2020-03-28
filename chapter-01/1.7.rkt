#lang racket

;; 1.7
(require "1.1.7-aux.rkt")

(sqrt 0.000001)

(define (good-enough-diff? guess x)
  (< (abs (- (improve guess x) guess)) 1e-10))
(define (sqrt-iter-diff guess x)
  (if (good-enough-diff? guess x)
      guess
      (sqrt-iter-diff (improve guess x) x)))

(sqrt-iter-diff 1.0 100)
(sqrt-iter-diff 1.0 0.000001)

