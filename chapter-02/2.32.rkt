#lang racket

;; 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (tl) (cons (car s) tl)) rest)))))

(subsets '(1 2 3))

