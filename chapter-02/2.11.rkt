#lang racket

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

