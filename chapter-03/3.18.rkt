#lang sicp

(define (last-pair x)
  (if (pair? x)
      (if (null? (cdr x))
          x
          (last-pair (cdr x)))
      nil))

(define (make-cycle x)
  (set-cdr! (last-pair x) x))

(define (has-cycle? x)
  (define (has-cycle-aux visited x)
    (if (pair? x)
        (if (memq (car x) visited)
            true
            (has-cycle-aux (cons (car x) visited) (cdr x)))
        false))
  (has-cycle-aux nil x))

(define l '(1 2 3 4 5))

;; (has-cycle? l)
;; (make-cycle l)
;; (has-cycle? l)

(#%provide make-cycle last-pair)
