#lang racket

(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(acc + 0 '(1 2 3 4 5))
(acc * 1 '(1 2 3 4 5))

(define (accn op init seqs)
  (if (null? (car seqs))
      '()
      (cons (acc op init (map car seqs))
            (accn op init (map cdr seqs)))))

(accn + 0 (list (list 1 2 3) (list 2 3 4)))

;; 2.37 Matrix Ops

(define mat
  (list (list 1 0 0 0)
        (list 0 1 0 0)
        (list 0 0 1 0)
        (list 0 0 0 1)))

(define (dot-product v w)
  (acc + 0 (map * v w)))

(dot-product (list 1 2 3 4) (list 2 3 4 5))

(define (mat-*-vec m v)
  (map (lambda x (dot-product (car x) v)) m))

(mat-*-vec mat (list 1 1 1 1))
