#lang sicp

(#%require rackunit)

(define (filtered-accumulate combiner
                             null-value
                             term
                             a
                             next
                             b
                             predicate)
  (define (combine result a)
    (if (predicate a)
        (combiner result (term a))
        result))

  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combine result a))))
  
  (iter a null-value))

(define (square x) (* x x))

(define (inc a) (+ a 1))

(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (next a)
    (if (= a 2)
        3
        (+ a 2)))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (define (smallest-divisor n)
    (find-divisor n 2))

  (= n (smallest-divisor n)))

(define (prime-squares-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (relatively-prime? a b)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

  (= (gcd a b) 1))

(define (id a) a)

(define (relatively-primes-product n)
  (define (relatively-prime-to-n? a)
    (relatively-prime? a n))

  (filtered-accumulate * 1 id 2 inc n relatively-prime-to-n?))

(check-equal? (prime-squares-sum 2 10) 87)
(check-equal? (relatively-primes-product 10) 189)
