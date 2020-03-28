#lang racket

(require "2.40.rkt")

(define (unique-triples n)
  (if (= n 2)
      '()
      (append (map (lambda (x) (cons n x))
                   (unique-pairs (- n 1)))
              (unique-triples (- n 1)))))

(define (fixed-sum-triples n sum)
  (filter (lambda (x) (= sum (apply + x)))
          (unique-triples n)))

(fixed-sum-triples 5 10)
