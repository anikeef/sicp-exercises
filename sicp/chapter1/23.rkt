#lang sicp

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next a)
  (if (= a 2)
      3
      (+ a 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
  true

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      false))

(define (search-for-first-primes a k)
  (cond ((= k 0) true)
        ((timed-prime-test a)
          (search-for-first-primes (+ a 1) (- k 1)))
        (else (search-for-first-primes (+ a 1) k))))

(newline)
(search-for-first-primes 1000 3)

(newline)
(search-for-first-primes 10000 3)

(newline)
(search-for-first-primes 100000 3)

(newline)
(search-for-first-primes 1000000 3)
