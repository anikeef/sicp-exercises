#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))

  (define (try x)
    (let ((next (f x)))
      (if (good-enough? x next)
          next
          (try next))))

  (try first-guess))

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(display phi)
(newline)
