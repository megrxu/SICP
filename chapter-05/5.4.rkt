#lang racket

(require "5.1.2-aux.rkt")

;; GCD Example

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

(set-register-contents! gcd-machine 'a 36)
(set-register-contents! gcd-machine 'b 48)
(start gcd-machine)
(get-register-contents gcd-machine 'a)

;; Fact Example

(define fact-machine
  (make-machine
   '(n continue val)
   (list (list '- -) (list '* *) (list '= =))
   '(controller
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done)))

(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(get-register-contents fact-machine 'val)

;; Fib Example

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '+ +) (list '- -) (list '< <))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     afterfib-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(get-register-contents fib-machine 'val)

;; Expt example

(define (expt-r b n)
  (if (= n 0)
      1
      (* b (expt-r b (- n 1)))))

(define exp-r-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '(controller
      (assign continue (label exp-done))
      exp-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-exp))
      (goto (label exp-loop))
      after-exp
      (restore n)
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      exp-done)))

(set-register-contents! exp-r-machine 'n 10)
(set-register-contents! exp-r-machine 'b 2)
(start exp-r-machine)
(get-register-contents exp-r-machine 'val)

(define exp-i-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
     (assign val (const 1))
     exp-loop
     (test (op =) (reg n) (const 0))
     (branch (label exp-done))
     (assign val (op *) (reg b) (reg val))
     (assign n (op -) (reg n) (const 1))
     (goto (label exp-loop))
     exp-done)))

(set-register-contents! exp-i-machine 'n 10)
(set-register-contents! exp-i-machine 'b 2)
(start exp-i-machine)
(get-register-contents exp-i-machine 'val)
