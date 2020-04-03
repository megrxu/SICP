#lang racket

(require "3.1.2-aux.rkt")

(define (estimate-integral p x1 x2 y1 y2)
  (monte-carlo 100000 (lambda () (p (random x1 x2) (random y1 y2)))))

(define p
  (lambda (x y) (<= (+ (* x x) (* y y)) 10000)))

(* 4 (estimate-integral p -100 100 -100 100))
