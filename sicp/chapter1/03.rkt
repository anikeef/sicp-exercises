#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (<= x y) (not (> x y)))

(define (less-than-two x y z)
  (and (<= x y) (<= x z)))

(define (greatest-sum-of-squares x y z)
  (cond ((less-than-two x y z) (sum-of-squares y z))
        ((less-than-two y x z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(check-equal? (greatest-sum-of-squares 1 2 3) 13)
(check-equal? (greatest-sum-of-squares 2 1 3) 13)
(check-equal? (greatest-sum-of-squares 3 2 1) 13)
