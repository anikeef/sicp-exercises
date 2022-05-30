#lang sicp

(#%require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multiply a b)
  (cond ((= b 1) a)
        ((even? b) (multiply (double a)
                             (halve b)))
        (else (+ a (multiply a (- b 1))))))

(check-equal? (multiply 7 12) 84)
