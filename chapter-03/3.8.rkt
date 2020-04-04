#lang racket

(define flag 1)
(define (f x)
  (if (zero? flag)
      0
      (cond [(= x 0) (begin (set! flag 0)
                            0)]
            [else x])))

(f 0)
(f 1)
