#lang racket

(require "3.13.rkt")

(define cdr mcdr)
(define car mcar)
(define list mlist)
(define cons mcons)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define caar (compose car car))
(define cadr (compose car cdr))

(provide cdr car list cons set-cdr! set-car!)
