#lang sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
  ((even? exp)
    (remainder (square (expmod base (/ exp 2) m)) m))
  (else
    (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

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
  (if (fast-prime? n 50)
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
