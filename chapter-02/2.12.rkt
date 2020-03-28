#lang racket

;; 2.12
(require "2.7.rkt")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i)) (/ (center i) 50.0)))

