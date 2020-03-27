#lang racket

;; 1.43

(define (repeated f times)
  (cond ((< times 0) error "Error")
        ((zero? times) id)
        (else (compose (repeated f (- times 1)) f))))
((repeated square 3) 2)

