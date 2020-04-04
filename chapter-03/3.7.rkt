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

(define (make-joint acc password new-password)
  (lambda (given-password method)
    (if (eq? given-password new-password)
        (acc password method)
        (lambda (_) "Incorrect password"))))

(define peter-acc (make-account 100 'secret))
(define paul-acc
  (make-joint peter-acc 'secret 'new-secret))

((peter-acc 'secret 'withdraw) 40)
((paul-acc 'new-secret 'deposit) 60)
((peter-acc 'secret 'withdraw) 40)
((paul-acc 'secret 'deposit) 60)
