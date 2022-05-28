#lang sicp

(#%require rackunit)

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? previous-guess guess)
  (< (/ (abs (- previous-guess guess)) guess) 0.001))

(define (sqrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
     guess
     (sqrt-iter guess
                (improve guess x)
                x)))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))

(sqrt 2)
(sqrt 1000000)
