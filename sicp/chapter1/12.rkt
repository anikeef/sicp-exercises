#lang sicp

(#%require rackunit)

(define (binomial n k)
  (if (or (= k 0) (= k n))
      1
      (+ (binomial (- n 1) k)
         (binomial (- n 1) (- k 1)))))

(check-equal? (binomial 10 5) 252)