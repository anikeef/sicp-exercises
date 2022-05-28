#lang sicp

(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (* 2 guess)
        (/ x (square guess)))
     3))

(define (diff x y) (abs (- x y)))

(define (good-enough? previous-guess guess)
  (< (/ (diff previous-guess guess)
        guess)
     0.001))

(define (cubic-root-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      guess
      (cubic-root-iter guess
                       (improve guess x)
                       x)))

(define (cubic-root x)
  (cubic-root-iter 1.0 (improve 1.0 x) x))

(display (cubic-root 0.000008))
(display "\n")
(display (cubic-root 1000000000))
(display "\n")
