#lang sicp

(define (iterative-improve good-enough? improve)
  (define (iter value)
    (let ((next (improve value)))
      (if (good-enough? value next)
        next
        (iter next))))
  iter)

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? previous-guess guess)
    (< (/ (abs (- previous-guess guess)) guess) 0.001))
  ((iterative-improve good-enough? improve) x))

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(display phi)
(newline)
(display (sqrt 2.0))
(newline)
