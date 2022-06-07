#lang sicp

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try x)
    (let ((next (f x)))
      (if (good-enough? x next)
          next
          (try next))))
  (try first-guess))

(define (fast-exp b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (square x) (* x x))
  (define (fast-exp-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-exp-iter a
                                    (square b)
                                    (/ n 2)))
          (else (fast-exp-iter (* a b)
                               b
                               (- n 1)))))
  (fast-exp-iter 1 b n))

(define (f c k)
  (lambda (x)
    (/ (+ (* (- k 1)
             x)
          (/ c
             (fast-exp x (- k 1))))
       k)))

(define (root c n)
  (fixed-point (f c n) 1.0))

(display (root 2 2))
