#lang racket

;; 2.23

(define (for-each items f)
  (if (null? items)
      (void)
      (begin (f (car items)) (for-each (cdr items) f))))

(for-each '(1 2 3 4) (lambda (x) (displayln x)))

