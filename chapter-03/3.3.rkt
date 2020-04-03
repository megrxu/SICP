#lang racket

(define (make-account amount password)
  (define balance amount)
  (lambda (given-password method)
    (if (eq? given-password password)
        (cond [(eq? method 'deposit)
               (lambda (x) (begin (set! balance (+ balance x))
                                  balance))]
              [(eq? method 'withdraw)
               (lambda (x) (if (> x balance)
                               "Insufficient funds"
                               (begin (set! balance (- balance x))
                                      balance)))]
              [else "Unknown method"])
        (lambda (_) "Incorrect password"))))

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 40)
((acc 'some-other 'deposit) 40)
