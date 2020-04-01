#lang racket

(require "2.61.rkt")

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([x1 (car set1)]
                    [x2 (car set2)])
                (cond [(= x1 x2) (cons x1 (union-set (cdr set1)
                                                     (cdr set2)))]
                      [(< x1 x2) (cons x1 (union-set (cdr set1)
                                                     set2))]
                      [(< x2 x1) (cons x2 (union-set (cdr set2)
                                                     set1))]))]))

;; (union-set '(1 3 5 7 9) '(2 4 6 8 10))

(provide union-set intersection-set)
