#lang racket

;; 2.4

(define (playground-2-4)
  (define (cons x y)
    (lambda (m) (m x y)))
  (define (car z)
    (z (lambda (p q) p)))
  (define (cdr z)
    (z (lambda (p q) q)))
  (displayln (car (cons 1 2)))
  (displayln (cdr (cons 1 2))))
(playground-2-4)

