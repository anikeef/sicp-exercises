#lang sicp

(define (sum term a next b)
  (define (iter result a)
    (if (> a b)
        result
        (iter (+ result (term a))
              (next a))))
  (iter 0 a))

(define (inc a) (+ a 1))

(define (integral f a b n)
  (define (coefficient k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))

  (define h (/ (- b a) n))

  (define (term k)
    (* (coefficient k)
       (f (+ a (* k h)))))
       
  (* (sum term 0 inc n)
     (/ h 3)))

(define (cube x) (* x x x))

(display (integral cube 0.0 1.0 100))
(newline)
(display (integral cube 0.0 1.0 1000))
(newline)
