#lang racket

;; 2.44

(define (make-vect c1 c2)
  (cons c1 c2))

(define (make-cor x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cdr)

(define (add-cor c1 c2)
  (make-cor (+ (car c1) (car c2))
            (+ (cdr c1) (cdr c2))))
(define (sub-cor c1 c2)
  (make-cor (- (car c1) (car c2))
            (- (cdr c1) (cdr c2))))
(define (scale-cor n c)
  (make-cor (* (car c) n)
            (* (cdr c) n)))

(define (add-vect v1 v2)
  (make-vect (add-cor (xcor-vect v1) (xcor-vect v2))
             (add-cor (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (sub-cor (xcor-vect v1) (xcor-vect v2))
             (sub-cor (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect n v)
  (make-vect (scale-cor n (xcor-vect v))
             (scale-cor n (ycor-vect v))))

(define v1
  (make-vect (make-cor 1.0 1.0)
             (make-cor 2.0 2.0)))
(define v2
  (make-vect (make-cor 2.0 2.0)
             (make-cor 3.0 3.0)))

(add-vect v1 v2)
(sub-vect v2 v1)
(scale-vect 2.5 v1)
