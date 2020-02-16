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

(define (test-prime n prime?)
  (let [(start (current-milliseconds))]
    (if (prime? n)
        (cons n (- (current-milliseconds) start))
        (test-prime (+ 2 n) prime?))))

(test-prime (+ 1000000000 1) prime-naive?)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (quotient exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime-fast? n)
  (fast-prime? n 1))

(test-prime (+ 1000000000 1) prime-fast?)

;; 1.23

(define (next-divisor n)
  (cond ((= n 2) 3)
        (else (+ 2 n))))
(define (smallest-divisor* n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next-divisor test-divisor)))))
  (define (divides? a b)
    (zero? (remainder b a)))
  (find-divisor n 2))
(define (prime-naive*? n)
  (= n (smallest-divisor* n)))
(test-prime (+ 1000000000 1) prime-naive*?)

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

(define (cube x)
  (* x x x))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

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

;; 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (id x) x)
(sum-iter id 1 inc 10)

;; 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact n)
  (product id 1 inc n))
(fact 0) ;; 0! = 1
(fact 5) ;; 5! = 120

(define (get-pi n)
  (* 4.0
     (product
      (lambda (i) (/ (* (inc i) (- i 1)) (square i)))
      3.0
      (lambda (x) (+ x 2))
      n)))

(square (/ (inc 3.0) 3.0))
(get-pi 1000000)

;; 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(accumulate + 0 id 1 inc 100) ;; 5050
(accumulate * 1 id 1 inc 5) ;; 120

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

;; 1.3.3 aux functions

(define (search f neg-point pos-point)
  (let [(midpoint (average neg-point pos-point))]
    (if (close-enough? neg-point pos-point)
        midpoint
        (let [(test-value (f midpoint))]
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let [(a-value (f a))
        (b-value (f b))]
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

(define tolerance 0.0000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt-fix x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
(sqrt-fix 100)

;; 1.35

(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0)

;; 1.36

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))
  (let [(res (try first-guess))]
    (newline)
    (display res)))
(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)
(fixed-point-print (lambda (x) (average (/ (log 1000) (log x)) x)) 2.0)

;; 1.37

(define (cont-frac-rec n d k)
  (define (component i)
    (cond ((= i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (component (+ i 1))) ))))
  (component 1))

(define (cont-frac n d k)
  (define (iter i res)
    (cond ((= i 0) res)
          (else (iter (- i 1) (/ (n i) (+ (d i) res))))) )
  (iter k 0))

(newline)
(cont-frac-rec (lambda (i) 1.0)
               (lambda (i) 1.0)
               10000)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10000)

;; 1.38

(+ 2
   (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (cond ((= (remainder i 3) 0) 1.0)
                      ((= (remainder i 3) 1) 1.0)
                      (else (* 2.0 (/ (inc i) 3)))))
              1000000)) ;; e

;; 1.39

(define (tan-cf x k)
  (cond ((= x 0) 0)
        (else
         (/ (cont-frac (lambda (i) (- (square x)))
                       (lambda (i) (- (* 2 i) 1))
                       k)
            (- x)))))

(tan-cf 0 100)
(tan-cf (/ pi 4) 1000)


;; 1.3.4 aux

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt-ad 100)


(define (cube-root-ad x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(cube-root-ad 125)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.000001)

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newton-method (lambda (y) (- (square y) x)) x))
(sqrt-nm 100)

;; 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))
(newton-method (cubic 1 -2 -30) 1)

;; 1.41

(define (double f)
  (lambda (x) (f (f x))))
((double inc) 2)

;; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

;; 1.43

(define (repeated f times)
  (cond ((< times 0) error "Error")
        ((zero? times) id)
        (else (compose (repeated f (- times 1)) f))))
((repeated square 3) 2)

;; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx)))
       3.0)))
(define n-smooth (lambda (n) (repeated smooth n)))
(((n-smooth 10) square) 10)

;; 1.45

(define (n-root n x)
  (define (power n)
    (lambda (x) (accumulate * 1 (lambda (y) x) 1 inc n)))
  (fixed-point ((repeated average-damp (- n 1))
                (lambda (y) (/ x ((power (- n 1)) y)))) 1.0))
(n-root 7 128)

;; 1.46

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)

(define (fixed-point-ii f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) first-guess))
(fixed-point-ii cos 1.0)

(define (sqrt-ii x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))
(sqrt-ii 100)

;; chapter-01 finished
