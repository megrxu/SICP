#lang racket

(require "3.13.rkt")

(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))

(define m1 (mlist 'a 'b 'c))
(define m2
  (let ([x (mcons 'a null)])
    (mlist x x)))
(define m3
  (let* ([x (mcons 'a null)]
         [y (mlist x x)])
    (mlist x  y)))
(define m4 (mlist 'a 'b 'c))

(make-cycle m4)
m1
(count-pairs m1)
m2
(count-pairs m2)
m3
(count-pairs m3)
m4
;; (count-pairs m4) ;; halt

(provide m1 m2 m3 m4)
