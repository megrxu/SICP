#lang racket

;; 1.23

(define (next-divisor n)
  (cond ((= n 2) 3)
        (else (+ 2 n))))
(define (smallest-divisor* n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next-divisor test-divisor)))))
  (define (divides? a b)
    (zero? (remainder b a)))
  (find-divisor n 2))
(define (prime-naive*? n)
  (= n (smallest-divisor* n)))
(test-prime (+ 1000000000 1) prime-naive*?)

