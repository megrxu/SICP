#lang racket

;; 1.3.4 aux

(require "1.1.7-aux.rkt")
(require "1.3.3-aux.rkt")
(require "1.29-aux.rkt")

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt-ad 100)


(define (cube-root-ad x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(cube-root-ad 125)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.000001)

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newton-method (lambda (y) (- (square y) x)) x))
(sqrt-nm 100)

(provide newton-method dx average-damp)
