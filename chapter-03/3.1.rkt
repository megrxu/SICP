#lang racket

(define (make-accumulater i)
  (define sum i)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

(define A (make-accumulater 5))

(A 10)
(A 10)

(define B (make-accumulater 10))

(B 15)
(B 20)

