#lang racket

;; 2.2

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (define (average x y)
    (/ (+ x y) 2.0))
  (make-point
   (average (x-point (start-segment segment))
            (x-point (end-segment segment)))
   (average (y-point (start-segment segment))
            (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (displayln ")"))

(define p1 (make-point 1.0 4.0))
(define p2 (make-point 7.0 -2.0))
(define s0 (make-segment p1 p2))
(print-point (midpoint-segment s0))

(provide make-point)
