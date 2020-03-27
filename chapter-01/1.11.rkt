#lang racket

;; 1.11

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (f-2 n)
  (define (f-iter x y z cnt)
    (cond ((= cnt 0) x)
          (else (f-iter y z (+ z (* 2 y) (* 3 x)) (- cnt 1)))))
  (f-iter 0 1 2 n))

(= (f-2 30) (f 30))

