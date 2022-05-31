#lang sicp

(#%require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fib-iter a b p q n)
  (cond ((= n 0) b)
        ((even? n) (fib-iter a
                             b
                             (+ (square p)
                                (square q))
                             (+ (square q)
                                (* 2 p q))
                             (/ n 2)))
        (else (fib-iter (+ (* a q)
                           (* b q)
                           (* a p))
                        (+ (* a q)
                           (* b p))
                        p
                        q
                        (- n 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 3) 2)
(check-equal? (fib 4) 3)
(check-equal? (fib 5) 5)
