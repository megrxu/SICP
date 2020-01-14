#lang racket

;; 1.3
(define (sum-max-two x y z)
  (cond ((and (< x y) (< x z)) (+ y z))
        ((and (< y z) (< y x)) (+ x z))
        ((and (< z x) (< z y)) (+ x y))))


(sum-max-two 1 2 3)
(sum-max-two 9 5 2)

;; Example 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 100)

;; 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x) x)))

;;(new-sqrt-iter 1.0 1) ;; halt 正则序和应用序的问题

;; 1.7
(sqrt 0.000001)

(define (good-enough-diff? guess x)
  (< (abs (- (improve guess x) guess)) 1e-10))
(define (sqrt-iter-diff guess x)
  (if (good-enough-diff? guess x)
      guess
      (sqrt-iter-diff (improve guess x) x)))

(sqrt-iter-diff 1.0 100)
(sqrt-iter-diff 1.0 0.000001)

;; 1.8
(define (cube-root-iter guess x)
  (if (good-enough-cr? guess x)
      guess
      (cube-root-iter (improve-cr guess x) x)))
(define (good-enough-cr? guess x)
  (< (abs (- (improve-cr guess x) guess)) 1e-10))
(define (improve-cr guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(cube-root-iter 1.0 1000.0)

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

;; 1.12

(define (pascal n)
  (define (next-list l)
    (reverse (list* 1 (map
                       (lambda (x y) (+ x y))
                       (cdr (reverse l))
                       (reverse (cdr l))))))
  (define (pascal-iter l1 l2 n)
    (cond ((= n 0) l1)
          (else (pascal-iter l2 (list* 1 (next-list l2)) (- n 1)))))
  (pascal-iter '(1) '(1 1) n))

(pascal 7)

;; 1.16

(define (expt b n)
  (define (iter a b n)
    (cond ((= n 0) 1)
          ((= n 1) (* a b))
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) (square b) (quotient n 2)))))
  (iter 1 b n))

(expt 5 3)
(expt 2 8)

;; 1.17-18

(define (fast-* a b)
  (define (double x) (+ x x))
  (define (iter r a b)
    (cond ((= b 0) 0)
          ((= b 1) (+ r a))
          ((even? b) (iter r (double a) (/ b 2)))
          (else (iter (+ r a) (double a) (quotient b 2)))))
  (iter 0 a b))

(fast-* 10 12)
(= (fast-* 1123 123) (* 1123 123))

;; 1.19

(define (fast-fib n)
  (define (once ab)
    (cons (+ (car ab) (cdr ab)) (car ab)))
  (define (twice ab)
    (once (once ab)))
  (define (iter ab n)
    (cond ((= n 0) (cdr ab))
          ((= n 1) (car ab))
          ((even? n) (iter (twice ab) (/ n 2)))
          (else (iter (once (twice ab)) (quotient n 2)))))
  (iter '(1 . 0) n))

(fast-fib 1e100) ;; very fast

;; 1.21

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (zero? (remainder b a)))
  (find-divisor n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; 1.22

(define (prime-naive? n)
  (equal? n (smallest-divisor n)))

(define (timed-prime-test n prime?)
  (let ((start (current-milliseconds)))
    (if (prime? n)
      (- (current-milliseconds) start)
      0
      )))


(timed-prime-test 10001 prime-naive?)
(timed-prime-test 100 prime-naive?)

(map (lambda n ((timed-prime-test (car n) prime-naive?))) (range 1 100))

;; 1.29 pre

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-sqrs a b)
  (sum square a inc b))
(sum-sqrs 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 10000))

