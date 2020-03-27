#lang racket

;; 2.30

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-high tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-high sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree '(1 (2 (3 4) 5)))
(square-tree-high '(1 (2 (3 4) 5)))

