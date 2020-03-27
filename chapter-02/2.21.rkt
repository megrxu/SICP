#lang racket

;; 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-prime items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))
(square-list-prime (list 1 2 3 4))

