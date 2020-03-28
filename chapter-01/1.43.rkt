#lang racket

;; 1.43
(require "1.30.rkt")
(require "1.1.7-aux.rkt")

(define (repeated f times)
  (cond ((< times 0) error "Error")
        ((zero? times) id)
        (else (compose (repeated f (- times 1)) f))))
((repeated square 3) 2)

(provide repeated)