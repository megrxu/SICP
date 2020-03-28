#lang racket

;; 2.42

(require "2.40.rkt")
(require "2.33-aux.rkt")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (let ([kp (car positions)]
        [rest (cdr positions)])
    (car (accumulate (lambda (p res)
                       (cons (and (car res)
                                  (and (not (= p kp))
                                       (not (= (abs (- p kp))
                                               (cdr res)))))
                             (- (cdr res) 1)))
                     (cons true (- k 1))
                     rest))))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(length (queens 8))
