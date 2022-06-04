#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (filter-nontrivial-root value squared-value m)
  (if (= squared-value 1)
      0
      squared-value))

(define (trivial-root? a m)
  (or (= a 1) (= a (- m 1))))

(define (squaremod a m)
  (if (trivial-root? a m)
      1
      (filter-nontrivial-root a
                              (remainder (square a) m)
                              m)))

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
          (squaremod (expmod a (/ n 2) m) m))
        (else
          (remainder (* a (expmod a (- n 1) m)) m))))

(define (miller-rabin-test n times)
  (define (not-prime? result)
    (not (= result 1)))

  (define (random-number)
    (+ (random (- n 2)) 1))

  (define (try times)
    (cond ((= times 0) true)
          ((not-prime? (expmod (random-number)
                               (- n 1)
                               n)) false)
          (try (- times 1))))

  (try times))

(define (prime? n)
  (miller-rabin-test n 50))

(check-pred prime? 7)
(check-pred prime? 61)
(check-pred prime? 101)
(check-equal? (prime? 100) false)
(check-equal? (prime? 64) false)
(check-equal? (prime? 98) false)
