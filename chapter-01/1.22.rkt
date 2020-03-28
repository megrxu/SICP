#lang racket

;; 1.22

(require "1.21.rkt" )
(require "1.1.7-aux.rkt" )

(define (prime-naive? n)
  (equal? n (smallest-divisor n)))

(define (test-prime n prime?)
  (let [(start (current-milliseconds))]
    (if (prime? n)
        (cons n (- (current-milliseconds) start))
        (test-prime (+ 2 n) prime?))))

(test-prime (+ 1000000000 1) prime-naive?)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (quotient exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime-fast? n)
  (fast-prime? n 1))

(test-prime (+ 1000000000 1) prime-fast?)

(provide test-prime prime-naive? prime-fast?)
