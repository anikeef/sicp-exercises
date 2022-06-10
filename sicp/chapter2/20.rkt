#lang sicp

(define (filter predicate)
  (define (filter-unary items)
    (if (null? items)
        nil
        (let ((first (car items))
              (rest (filter-unary (cdr items))))
          (if (predicate first)
              (cons first rest)
              rest))))
  filter-unary)

(define (same-parity x . rest)
  (define (predicate y)
    (= (remainder x 2) (remainder y 2)))
  (cons x ((filter predicate) rest)))

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
