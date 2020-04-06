#lang racket

(define (count-pairs x)
  (car
    (let aux ([x x]
             [visited '()])
      (if (and (pair? x) (not (memq x visited)))
        (let* ([p-car (aux (car x) (cons x visited))]
               [p-cdr (aux (cdr x) (cdr p-car))])
          (cons (+ (car p-car) (car p-cdr) 1) (cdr p-cdr)))
        (cons 0 visited)))))

(define m1 (list 'a 'b 'c))
(define m2
  (let ([x (list 'a)])
    (list x x)))
(define m3
  (let* ([x (list 'a)]
         [y (cons x x)])
    (cons x y)))

m1
(count-pairs m1)
m2
(count-pairs m2)
m3
(count-pairs m3)
