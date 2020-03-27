#lang racket

;; 1.19

(define (fast-fib n)
  (define (once ab)
    (cons (+ (car ab) (cdr ab)) (car ab)))
  (define (twice ab)
    (once (once ab)))
  (define (iter ab n)
    (cond ((= n 0) (cdr ab))
          ((= n 1) (car ab))
          ((even? n) (iter (twice ab) (/ n 2)))
          (else (iter (once (twice ab)) (quotient n 2)))))
  (iter '(1 . 0) n))

(fast-fib 1e100) ;; very fast

