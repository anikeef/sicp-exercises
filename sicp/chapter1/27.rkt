#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
          (remainder (square (expmod a (/ n 2) m)) m))
        (else
          (remainder (* a (expmod a (- n 1) m)) m))))

(define (is-fermatic a n)
  (= (expmod a n n) a))

(define (fermat-test a)
  (define (try x)
    (cond ((= x a) true)
          ((is-fermatic x a) (try (+ x 1)))
          (else false)))
  (try 2))

(check-pred fermat-test 561)
(check-pred fermat-test 1105)
(check-pred fermat-test 1729)
(check-pred fermat-test 2465)
(check-pred fermat-test 2821)
(check-pred fermat-test 6601)
