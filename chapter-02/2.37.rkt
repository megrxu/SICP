#lang racket

;; 2.37
(require "2.33-aux.rkt")
(require "2.36.rkt")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix '((1 2 3) (4 5 6))
                 '((7 8) (9 10) (11 12)))

