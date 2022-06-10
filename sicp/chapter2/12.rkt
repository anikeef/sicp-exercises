#lang sicp

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

; Solution

(define (make-center-percent center deviation)
  (make-center-width center
                     (+ (* deviation center) center)))

(define (percent i)
  (/ (width i) (center i)))
