#lang racket

;; 2.1
(define (make-rat n d) (let [(g (gcd n d))]
                         (if (positive? d)
                             (cons (/ n g) (/ d g))
                             (cons (- (/ n g)) (- (/ d g))))))

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
  (display (numer x))
  (display "/")
  (displayln (denom x)))

(print-rat
 (mul-rat
  (make-rat -2 3)
  (make-rat 1 -4))) ;; -2/3 * 1/(-4) = 1/6
