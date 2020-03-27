#lang racket

;; 2.3
(define (make-rect point rect-length rect-width)
  (cons point (cons rect-length rect-width)))
(define (rect-length rect)
  (cadr rect))
(define (rect-width rect)
  (cddr rect))
(define (rect-aera rect)
  (* (rect-length rect)
     (rect-width rect)))
(define (rect-perimeter rect)
  (* 2 (+ (rect-length rect)
          (rect-width rect))))
(define rect (make-rect (make-point 2.0 3.0)
                           9
                           4.0))
(rect-perimeter rect)
(rect-aera rect)

