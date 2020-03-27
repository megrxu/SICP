#lang racket

;; 2.39

(define (reverse-1 l)
  (foldr (lambda (x y) (append y (list x))) '() l))
(define (reverse-2 l)
  (foldl cons '() l))

(reverse-1 '(1 2 3))
(reverse-2 '(1 2 3))

