#lang typed/racket

;; 1.19
(: fast-fib (-> Integer Integer))
(define (fast-fib n)
  (: square (-> Integer Integer))
  (define (square x)
    (* x x))
  (: fib-iter (-> Integer Integer Integer Integer Integer Integer))
  (define (fib-iter a b p q i)
   (cond ((= i 0) b)
         ((even? i)
          (fib-iter a
                    b
                    (+ (square p) (square q))
                    (+ (* 2 p q) (square q))
                    (quotient i 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- i 1)))))
  (fib-iter 1 0 0 1 n))
(fast-fib (exact-round 1000)) ;; very fast
