#lang racket

;; 1.29

(define (integral-simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (f (+ a (* h k))))
    (define (f* k)
      (cond ((= 0 k) (y k))
            ((= n k) (y k))
            ((even? k) (* 2 (y k)))
            (else (* 4 (y k)))))
    (* (/ h 3)
       (sum f* 0 inc n))))

(integral-simpson cube 0 1 100)
(integral-simpson square 0 1 1000)

