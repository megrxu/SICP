#lang racket

;; 1.31
(require "1.30.rkt" )
(require "1.29-aux.rkt" )
(require "1.1.7-aux.rkt" )

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact n)
  (product id 1 inc n))
(fact 0) ;; 0! = 1
(fact 5) ;; 5! = 120

(define (get-pi n)
  (* 4.0
     (product
      (lambda (i) (/ (* (inc i) (- i 1)) (square i)))
      3.0
      (lambda (x) (+ x 2))
      n)))

(square (/ (inc 3.0) 3.0))
(get-pi 1000000)

