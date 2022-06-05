#lang sicp

(#%require rackunit)

(define (accumulate combiner
                    null-value
                    term
                    a
                    next
                    b)

  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (id a) a)

(define (inc a) (+ a 1))

(check-equal? (sum id 1 inc 5) 15)
(check-equal? (product id 1 inc 5) 120)
