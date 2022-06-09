#lang sicp

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Solution

(define (negate interval)
  (make-interval (- (upper-bound interval))
                 (- (lower-bound interval))))

(define (sub-interval x y)
  (add-interval x
                (negate y)))
