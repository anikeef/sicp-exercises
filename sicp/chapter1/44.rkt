#lang sicp

(#%require rackunit)

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (compose f result))))
  (iter n id))

(define (smooth-repeated f n)
  ((repeated smooth n) f))
