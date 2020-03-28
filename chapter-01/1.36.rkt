#lang racket

;; 1.36
(require "1.3.3-aux.rkt")
(require "1.1.7-aux.rkt")

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))
  (let [(res (try first-guess))]
    (newline)
    (display res)))
(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)
(fixed-point-print (lambda (x) (average (/ (log 1000) (log x)) x)) 2.0)

