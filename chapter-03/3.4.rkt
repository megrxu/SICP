#lang racket

(define (call-the-cops)
  "Called the cops")

(define (make-account amount password)
  (define balance amount)
  (define count 0)
  (lambda (given-password method)
    (if (eq? given-password password)
        (begin (set! count 0)
               (cond [(eq? method 'deposit)
                      (lambda (x) (begin (set! balance (+ balance x))
                                         balance))]
                     [(eq? method 'withdraw)
                      (lambda (x) (if (> x balance)
                                      "Insufficient funds"
                                      (begin (set! balance (- balance x))
                                             balance)))]
                     [else "Unknown method"]))
        (if (< count 7)
            (begin (set! count (+ 1 count))
                   (lambda (_) "Incorrect password"))
            (lambda (_) (call-the-cops))))))

(define acc (make-account 100 'secret))

((acc 'secret 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secret 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
((acc 'secre 'deposit) 100)
