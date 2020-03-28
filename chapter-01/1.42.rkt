#lang racket

;; 1.42
(require "1.1.7-aux.rkt" "1.29-aux.rkt")

(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

