#lang sicp

(define (has-cycle? x)
  (define (one-step x)
    (if (pair? x)
        (cdr x)
        nil))
  (define (two-steps x)
    (if (and (pair? x) (pair? (cdr x)))
        (cddr x)
        nil))
  (define (chase a b)
    (cond [(null? a) false]
          [(eq? a b) true]
          [else (chase (one-step a) (two-steps b))]))
  (chase (one-step x) (two-steps x)))

(define l '(1 2 3 4 5))
(has-cycle? l)
(set-cdr! (cddddr l) l)
(has-cycle? l)
