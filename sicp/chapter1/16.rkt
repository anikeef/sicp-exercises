#lang sicp

(#%require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-exp b n)
  (define (fast-exp-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-exp-iter a
                                    (square b)
                                    (/ n 2)))
          (else (fast-exp-iter (* a b)
                               b
                               (- n 1)))))
  (fast-exp-iter 1 b n))

(check-equal? (fast-exp 2 12) 4096)
