#lang racket

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

