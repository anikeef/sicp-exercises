#lang sicp

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (negate-vect v)
  (make-vect (- (xcor-vect v))
             (- (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1 (negate-vect v2)))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
