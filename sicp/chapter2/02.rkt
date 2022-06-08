#lang sicp

(define (make-segment a b) (cons a b))
(define start-segment car)
(define end-segment cdr)

(define (make-point x y) (cons x y))
(define x-point car)
(define y-point cdr)

(define (average x y) (/ (+ x y) 2))

(define (segment-average a selector)
  (average (selector (start-segment a))
           (selector (end-segment a))))

(define (midpoint-segment a)
  (make-point (segment-average a x-point)
              (segment-average a y-point)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define a (make-point -1 2))
(define b (make-point 1 4))
(define segment (make-segment a b))

(print-point (midpoint-segment segment))
