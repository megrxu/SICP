#lang racket

(require "2.3-huffman.rkt")
(require "2.67.rkt")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(eq? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
          '()
          (error "invalid tree -- ENCODE-SYMBOL"))
      (let ([left (left-branch tree)]
            [right (right-branch tree)])
        (cond [(element-of-set? symbol (symbols left))
               (cons 0 (encode-symbol symbol left))]
              [(element-of-set? symbol (symbols right))
               (cons 1 (encode-symbol symbol right))]
              [else (error "invalid symbol -- ENCODE-SYMBOL")]))))

(define sample-symbols '(A D A B B C A))
;; (encode sample-symbols sample-tree)
;; sample-message

