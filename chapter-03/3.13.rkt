#lang racket

(define (make-cycle x)
  (set-mcdr! (last-mpair x) x))

(define (mlist . xs)
  (foldr mcons null xs))

(define (last-mpair ml)
  (if (mpair? ml)
      (if (null? (mcdr ml))
          ml
          (last-mpair (mcdr ml)))
      null))

(define example (mlist 'a 'b 'c))

(make-cycle example)

(provide make-cycle mlist last-mpair)
