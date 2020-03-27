#lang racket

;; 1.8
(define (cube-root-iter guess x)
  (if (good-enough-cr? guess x)
      guess
      (cube-root-iter (improve-cr guess x) x)))
(define (good-enough-cr? guess x)
  (< (abs (- (improve-cr guess x) guess)) 1e-10))
(define (improve-cr guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(cube-root-iter 1.0 1000.0)

