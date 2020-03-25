#lang racket

;; 2.1.1 aux

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (denom x) (numer y))
     (* (numer x) (denom y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (displayln (denom x)))

;; 2.1
(define (make-rat n d) (let [(g (gcd n d))]
                         (if (positive? d)
                             (cons (/ n g) (/ d g))
                             (cons (- (/ n g)) (- (/ d g))))))
(print-rat
 (mul-rat
  (make-rat -2 3)
  (make-rat 1 -4))) ;; -2/3 * 1/(-4) = 1/6

;; 2.2

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (define (average x y)
    (/ (+ x y) 2.0))
  (make-point
   (average (x-point (start-segment segment))
            (x-point (end-segment segment)))
   (average (y-point (start-segment segment))
            (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (displayln ")"))

(define p1 (make-point 1.0 4.0))
(define p2 (make-point 7.0 -2.0))
(define s0 (make-segment p1 p2))
(print-point (midpoint-segment s0))


;; 2.3
(define (make-rect point rect-length rect-width)
  (cons point (cons rect-length rect-width)))
(define (rect-length rect)
  (cadr rect))
(define (rect-width rect)
  (cddr rect))
(define (rect-aera rect)
  (* (rect-length rect)
     (rect-width rect)))
(define (rect-perimeter rect)
  (* 2 (+ (rect-length rect)
          (rect-width rect))))
(define rect (make-rect (make-point 2.0 3.0)
                           9
                           4.0))
(rect-perimeter rect)
(rect-aera rect)

;; 2.4

(define (playground-2-4)
  (define (cons x y)
    (lambda (m) (m x y)))
  (define (car z)
    (z (lambda (p q) p)))
  (define (cdr z)
    (z (lambda (p q) q)))
  (displayln (car (cons 1 2)))
  (displayln (cdr (cons 1 2))))
(playground-2-4)

;; 2.5

(define (playground-2-5)
  (define (cons a b)
    (* (expt 2 a)
       (expt 3 b)))
  (define (car z)
    (cond ((= (modulo z 2) 0) (+ (car (/ z 2)) 1))
          (else 0)))
  (define (cdr z)
    (cond ((= (modulo z 3) 0) (+ (cdr (/ z 3)) 1))
          (else 0)))
  (displayln (cons 1 2))
  (displayln (car (cons 1 2)))
  (displayln (cdr (cons 1 2))))
(playground-2-5)

;; 2.6

(define (playground-2-6)
  (define zero (lambda (f) (lambda (x) x)))
  (define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))
  (define one
    (lambda (f) (lambda (x) (f x))))
  (define two
    (lambda (f) (lambda (x) (f (f x)))))
  (define (plus x y)
    (lambda (f) (lambda (x) ((x f) ((y f) x)))))
  (display "Church"))

;; 2.7

(define (make-interval a b) (cons a b))

(define lower-bound car)
(define upper-bound cdr)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let [(p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; 2.8

(define (sub-interval x y)
  (let [(p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y)))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; 2.10

(define (div-interval x y)
  (let [(yl (lower-bound y))
        (yu (upper-bound y))]
    (if (positive? (* yl yu))
        (mul-interval x
                      (make-interval (/ 1.0 yu)
                                     (/ 1.0 yl)))
        (error "Div failed."))))

;; 2.11

(define (mul-interval-cases x y)
  (let ((yl (lower-bound y))
        (xl (lower-bound x))
        (yu (upper-bound y))
        (xu (upper-bound x)))
    (cond ((> xl 0) (cond ((> yl 0) (make-interval (* xl yl) (* xu yu)))
                          ((< yu 0) (make-interval (* xl yu) (* xu yl)))
                          (else (make-interval (* xu yl) (* xu yu)))))
          ((< xu 0) (cond ((> yl 0) (make-interval (* xu yl) (* xl yu)))
                          ((< yu 0) (make-interval (* yu xu) (* yl xl)))
                          (else (make-interval (* xl yu) (* xl yl)))))
          (else (cond ((> yl 0) (make-interval (* xl yu) (* xu yu)))
                      ((< yu 0) (make-interval (* xu yl) (* xl yl)))
                      (else (make-interval (min (* yu xl) (* yl xu)) (max (* yl xl) (* yu xu)))))))))

;; 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i)) (/ (center i) 50.0)))

;; 2.17

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(last-pair (list 1 2 3 4))

;; 2.18

(define (reverse l)
  (define (reverse-iter hd tl)
    (if (null? tl)
        hd
        (reverse-iter (cons (car tl) hd) (cdr tl))))
  (reverse-iter '() l))

(reverse (list 1 2 3 4 5))

;; 2.20

(define (same-parity x . y)
  (filter (lambda (i) (even? (- i x))) y))

(same-parity 1 2 3 4 5 6 7 8 9)
(same-parity 1 -3 -9 1 0 3 7)

;; 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-prime items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))
(square-list-prime (list 1 2 3 4))

;; 2.23

(define (for-each items f)
  (if (null? items)
      (void)
      (begin (f (car items)) (for-each (cdr items) f))))

(for-each '(1 2 3 4) (lambda (x) (displayln x)))

;; 2.25

(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadar (cddadr (cadadr '(1 (2 (3 (4 5 (6 7))))))))


;; 2.27

(define (deep-reverse l)
  (define (deep-reverse-iter hd tl)
    (if (null? tl)
        hd
        (if (list? (car tl))
            (deep-reverse-iter (cons (deep-reverse-iter '() (car tl)) hd)
                               (cdr tl))
            (deep-reverse-iter (cons (car tl) hd) (cdr tl)))))
  (deep-reverse-iter '() l))
