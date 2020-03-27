#lang racket

;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a)

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b)

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (branch-weight branch)
    (if (list? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))

(define em
  (make-mobile
   (make-branch 2 16)
   (make-branch 4 (make-mobile
                   (make-branch 2 6)
                   (make-branch 6 2)))))

(total-weight em)

; c)

(define (mobile-balance? mobile)
  (let ([lb (left-branch mobile)]
        [rb (right-branch mobile)])
    (and (or (not (list? (branch-structure lb)))
             (mobile-balance? (branch-structure lb)))
         (or (not (list? (branch-structure rb)))
             (mobile-balance? (branch-structure rb)))
         (= (* (branch-length lb) (branch-weight lb))
            (* (branch-length rb) (branch-weight rb))))))

(mobile-balance? em)

