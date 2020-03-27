#lang racket

;; 1.3
(define (sum-max-two x y z)
  (cond ((and (< x y) (< x z)) (+ y z))
        ((and (< y z) (< y x)) (+ x z))
        ((and (< z x) (< z y)) (+ x y))))


(sum-max-two 1 2 3)
(sum-max-two 9 5 2)

