#lang racket

(require "2.3-huffman.rkt")

(define (generate-huffman-tree pairs)
  (sucessive-merge (make-leaf-set pairs)))

(define (sort-asc pairs)
  (define (insert-ele x pairs)
    (cond [(null? pairs) (list x)]
          [(> (weight x)
              (weight (car pairs))) (cons (car pairs)
                                               (insert-ele x
                                                           (cdr pairs)))]
          [else (cons x pairs)]))
  (define (sort-aux res pairs)
    (if (null? pairs)
        res
        (sort-aux (insert-ele (car pairs) res) (cdr pairs))))
  (sort-aux '() pairs))

(define (sucessive-merge leaves)
  (define sorted (sort-asc leaves))
  (if (< (length leaves) 2)
      leaves
      (sucessive-merge (cons (make-code-tree (car sorted)
                                             (cadr sorted))
                             (cddr leaves)))))

(sort-asc (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
