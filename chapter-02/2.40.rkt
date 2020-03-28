#lang racket

(require (rename-in "../chapter-01/1.22.rkt"
                    (prime-naive? prime?)))
(require "2.33-aux.rkt")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval m n) (range m (+ 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; 2.40

(define (unique-pairs n)
  (if (= n 1)
      '()
      (append (map (lambda (x) (list n x)) (range 1 n))
              (unique-pairs (- n 1)))))

(unique-pairs 6)

(define (prime-sum-pairs-2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs-2 6)
(prime-sum-pairs 6)

(provide unique-pairs permutations flatmap enumerate-interval)
