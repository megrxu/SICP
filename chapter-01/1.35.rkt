#lang racket

;; 1.35
(require "1.3.3-aux.rkt")

(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0)

