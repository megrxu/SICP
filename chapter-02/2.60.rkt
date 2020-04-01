#lang racket

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define adjoin-set cons)

(define union-set append)

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) null]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

(intersection-set '(1 2 3 4 4 5) '(4 4 3 3 2))
