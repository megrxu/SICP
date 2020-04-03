#lang racket

;; 2.10

(define (div-interval x y)
  (let [(yl (lower-bound y))
        (yu (upper-bound y))]
    (if (positive? (* yl yu))
        (mul-interval x
                      (make-interval (/ 1.0 yu)
                                     (/ 1.0 yl)))
        (error "Div failed."))))

(provide div-interval)
(div 1 2)
