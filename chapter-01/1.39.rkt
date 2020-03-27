#lang racket

;; 1.39

(define (tan-cf x k)
  (cond ((= x 0) 0)
        (else
         (/ (cont-frac (lambda (i) (- (square x)))
                       (lambda (i) (- (* 2 i) 1))
                       k)
            (- x)))))

(tan-cf 0 100)
(tan-cf (/ pi 4) 1000)


