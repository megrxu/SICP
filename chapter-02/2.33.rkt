#lang racket

;; 2.33
(require "2.33-aux.rkt")

(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))
(map-2 (lambda (x) (+ x 1)) '(1 2 3 4))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))
(append-2 '(1 2 3 4) '(5 6 7))

(define (length-2 sequence)
  (accumulate (lambda (_ y) (+ 1 y))
              0
              sequence))
(length-2 '(1 2 3 4 5 6 7))

