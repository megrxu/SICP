#lang racket

;; 2.1
(define (make-rat n d) (let [(g (gcd n d))]
                         (if (positive? d)
                             (cons (/ n g) (/ d g))
                             (cons (- (/ n g)) (- (/ d g))))))
(print-rat
 (mul-rat
  (make-rat -2 3)
  (make-rat 1 -4))) ;; -2/3 * 1/(-4) = 1/6

