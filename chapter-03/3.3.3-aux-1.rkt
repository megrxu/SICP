#lang racket

(require "3.3-mpairs.rkt")

(define caar (compose car car))

(define (lookup key table)
  (let ([record (assoc key (cdr table))])
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond [(null? records) false]
        [(equal? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

(define (insert! key value table)
  (let ([record (assoc key (cdr table))])
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

(define t (make-table))
(insert! 'a 'cool t)
(insert! 'b 'not-cool t)

(lookup 'a t)

(insert! 'a 'not-cool t)

(lookup 'a t)
