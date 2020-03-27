#lang racket

;; 1.37

(define (cont-frac-rec n d k)
  (define (component i)
    (cond ((= i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (component (+ i 1))) ))))
  (component 1))

(define (cont-frac n d k)
  (define (iter i res)
    (cond ((= i 0) res)
          (else (iter (- i 1) (/ (n i) (+ (d i) res))))) )
  (iter k 0))

(newline)
(cont-frac-rec (lambda (i) 1.0)
               (lambda (i) 1.0)
               10000)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10000)

