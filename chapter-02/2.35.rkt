#lang racket

;; 2.35

(define (count-leaves t)
  (accumulate (lambda (x y)
                (if (pair? x)
                    (+ y (count-leaves x))
                    (+ y 1)))
              0
              t))
(count-leaves '(1 2 3 (2 4 (5 7) 8)))

