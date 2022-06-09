#lang sicp

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))

(define (reverse x)
  (if (and (<= (lower-bound x) 0)
           (>= (upper-bound x) 0))
      (error "Intervals that include 0 cannot be reversed")
      (make-interval (/ 1 (upper-bound x))
                     (/ 1 (lower-bound x)))))
