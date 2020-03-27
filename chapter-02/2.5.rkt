#lang racket

;; 2.5

(define (playground-2-5)
  (define (cons a b)
    (* (expt 2 a)
       (expt 3 b)))
  (define (car z)
    (cond ((= (modulo z 2) 0) (+ (car (/ z 2)) 1))
          (else 0)))
  (define (cdr z)
    (cond ((= (modulo z 3) 0) (+ (cdr (/ z 3)) 1))
          (else 0)))
  (displayln (cons 1 2))
  (displayln (car (cons 1 2)))
  (displayln (cdr (cons 1 2))))
(playground-2-5)

