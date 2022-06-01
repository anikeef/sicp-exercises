#lang sicp

(#%require rackunit)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a n)
  (= (remainder n a) 0))

(define (square x) (* x x))

(define (find-divisor n a)
  (cond ((> (square a) n) n)
        ((divides? a n) a)
        (else (find-divisor n (+ a 1)))))

(check-equal? (smallest-divisor 199) 199)
(check-equal? (smallest-divisor 1999) 1999)
(check-equal? (smallest-divisor 19999) 7)
