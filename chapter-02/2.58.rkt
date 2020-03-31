#lang racket

;; 2.58

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

(define (mid-to-pre exp)
  (match exp
    [(list a '+ bs ...)
     (list '+ (mid-to-pre a) (mid-to-pre bs))]
    [(list a '* bs ...)
     (list '* (mid-to-pre a) (mid-to-pre bs))]
    [(list a) (mid-to-pre a)]
    [a a]))

(define (pre-to-mid exp)
  (match exp
    [(list op as ...)
     (cdr (apply append
                 (map (lambda (x) (list op (pre-to-mid x))) as)))]
    [(list c) (pre-to-mid c)]
    [c c]))

(mid-to-pre '(x + y + (z * 1) + 3))
(pre-to-mid '(+ x (* y 8) z))
((compose pre-to-mid mid-to-pre) '(x + y + (z * 1) + 3))

(define (deriv-mid exp var)
  (pre-to-mid (deriv (mid-to-pre exp) var)))

(deriv-mid '(x + y * y + (7 * x)) 'y)

;; kind of tricky, but works...
;; did not deal with the order of operations
