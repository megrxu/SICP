#lang racket

(require "3.13.rkt")

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ([temp (mcdr x)])
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))


(mystery (mlist 'a 'b 'c 'd))
