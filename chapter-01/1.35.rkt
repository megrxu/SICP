#lang racket

;; 1.35

(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0)

