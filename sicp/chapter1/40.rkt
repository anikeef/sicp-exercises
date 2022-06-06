#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(check-equal? ((cubic 1 2 3) 1) 7)
