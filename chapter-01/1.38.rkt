#lang racket

;; 1.38

(require "1.37.rkt")
(require "1.29-aux.rkt")

(+ 2
   (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (cond ((= (remainder i 3) 0) 1.0)
                      ((= (remainder i 3) 1) 1.0)
                      (else (* 2.0 (/ (inc i) 3)))))
              1000000)) ;; e

