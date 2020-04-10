#lang racket

(require "3.13.rkt")

(define (make-queue)
  (mcons null null))
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define set-front-ptr! set-mcar!)
(define set-rear-ptr! set-mcdr!)

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (mlist item)])
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE called with an empty queue" queue)]
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

;; (define q (make-queue))
;; (insert-queue! q 'a)
;; (insert-queue! q 'b)
;; (insert-queue! q 'c)
;; (front-queue q)
;; (delete-queue! q)
;; (insert-queue! q 'd)
(provide make-queue delete-queue! insert-queue! front-queue empty-queue?)
