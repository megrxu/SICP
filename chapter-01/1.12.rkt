#lang racket

;; 1.12

(define (pascal n)
  (define (next-list l)
    (reverse (list* 1 (map
                       (lambda (x y) (+ x y))
                       (cdr (reverse l))
                       (reverse (cdr l))))))
  (define (pascal-iter l1 l2 n)
    (cond ((= n 0) l1)
          (else (pascal-iter l2 (list* 1 (next-list l2)) (- n 1)))))
  (pascal-iter '(1) '(1 1) n))

(pascal 7)

