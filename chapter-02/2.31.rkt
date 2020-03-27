#lang racket

;; 2.31

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(tree-map (lambda (x) (* x x)) '(1 (2 (3 4) 5)))


