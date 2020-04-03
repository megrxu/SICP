#lang racket

(define (make-monitored f)
  (define count 0)
  (lambda (x)
    (cond [(eq? 'how-many-calls? x) count]
          [(eq? 'reset-count x) (set! count 0)]
          [(number? x) (begin (set! count (+ count 1))
                              (f x))]
          [else (error "invalid input")])))

(define s (make-monitored sqrt))

;; (s 100)
;; (s 81)
;; (s 2.25)
;; (s 'how-many-calls?)

(provide make-monitored)
