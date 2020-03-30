#lang racket

;; 2.57

(require "2.3.2-aux.rkt")

(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define multiplier cadr)
(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

(partition number? '(1 2 3 x y))

(define (make-sum a1 . a2)
  (let-values (([nums oths] (partition number? (cons a1 a2))))
    (if (null? oths)
        (apply + nums)
        (let ((num (apply + nums))
              (seq oths))
          (cond ((and (null? (cdr seq)) (= num 0)) (car seq))
                ((null? (cdr seq)) (list '+ num (car seq)))
                ((and (pair? seq) (= num 0) (cons '+ seq)))
                ((pair? seq) (append (list '+ num) seq))
                (else (list '+ num seq)))))))

(define (make-product m1 . m2)
  (let-values (([nums oths] (partition number? (cons m1 m2))))
    (if (null? oths)
        (apply * nums)
        (let ((num (apply * nums))
              (seq oths))
          (cond ((= num 0) 0)
                ((and (null? (cdr seq)) (= num 1)) (car seq))
                ((null? (cdr seq)) (list '* num (car seq)))
                ((and (pair? seq) (= num 1) (cons '* seq)))
                ((pair? seq) (append (list '* num) seq))
                (else (list '* num seq)))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(make-product 'x 'y (make-sum 'x 3))
