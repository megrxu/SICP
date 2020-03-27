#lang racket

;; 1.33

(define (filtered-accumulate filter-fun combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter-fun (term a)) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(filtered-accumulate (lambda (x) #t) + 0 id 1 inc 100)
(filtered-accumulate prime-naive? + 0 id 10 inc 20) ;; 11 + 13 + 17 + 19 = 60

(define (quiz-2 n)
  (filtered-accumulate (lambda (x) (= (gcd x n) 1))
                       * 1 id 1 inc n))
(= (quiz-2 10) (* 1 3 7 9)) ;; 1 * 3 * 7 * 9 = 189

