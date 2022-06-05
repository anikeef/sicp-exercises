#lang sicp

(#%require rackunit)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))

  (iter a 1))

(define (id a) a)

(define (inc a) (+ a 1))

(define (factorial n)
  (product id 1 inc n))

(define (even? n)
  (= (remainder n 2) 0))

(define (pi terms)
  (define (numerator k)
    (if (even? k)
        (+ k 2)
        (+ k 1)))
  
  (define (denominator k)
    (if (even? k)
        (+ k 1)
        (+ k 2)))
  
  (define (term k)
    (/ (numerator k) (denominator k)))

  (* 4 (product term 1.0 inc terms)))

(check-equal? (factorial 5) 120)

(display (pi 100000))
(newline)
