#lang racket

;; 2.18

(define (reverse l)
  (define (reverse-iter hd tl)
    (if (null? tl)
        hd
        (reverse-iter (cons (car tl) hd) (cdr tl))))
  (reverse-iter '() l))

(reverse (list 1 2 3 4 5))

