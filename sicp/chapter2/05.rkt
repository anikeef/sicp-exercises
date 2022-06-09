#lang sicp

(#%require rackunit)

(define (divides? x n)
  (= (remainder n x) 0))

(define (fast-exp b n)
  (define (even? n) (divides? 2 n))
  (define (square x) (* x x))
  (define (fast-exp-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-exp-iter a
                                    (square b)
                                    (/ n 2)))
          (else (fast-exp-iter (* a b)
                               b
                               (- n 1)))))
  (fast-exp-iter 1 b n))

(define (make-pair a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (find-power divisor)
  (define (iter result a)
    (if (divides? divisor a)
        (iter (+ result 1) (/ a divisor))
        result))
  (lambda (x) (iter 0 x)))

(define car (find-power 2))
(define cdr (find-power 3))

(define pair (make-pair 10 17))
(check-equal? (car pair) 10)
(check-equal? (cdr pair) 17)
