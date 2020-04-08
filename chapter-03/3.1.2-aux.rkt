#lang racket

(define (rand)
  (random (expt 2 16)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1.0))]
          [else
           (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

;; (estimate-pi 100000.0)

(provide monte-carlo rand)
