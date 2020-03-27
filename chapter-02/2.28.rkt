#lang racket

;; 2.28

(define (fringe l)
  (define (fringe-iter leaves tl)
    (if (null? tl)
        leaves
        (if (list? (car tl))
            (fringe-iter (append (fringe-iter '() (car tl)) leaves) (cdr tl))
            (fringe-iter (cons (car tl) leaves) (cdr tl)))))
  (reverse (fringe-iter '() l)))

(fringe '((1 2) (2 3) (3 (4 (4 5)))))

