#lang sicp

(#%require rackunit)

(define (id x) x)
(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (compose f result))))
  (iter n id))

(check-equal? ((repeated square 2) 5) 625)
