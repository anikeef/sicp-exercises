#lang sicp

; Helpers

(define (iterative-improve good-enough? improve)
  (define (iter value)
    (let ((next (improve value)))
      (if (good-enough? value next)
        next
        (iter next))))
  iter)

(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? previous-guess guess)
    (< (/ (abs (- previous-guess guess)) guess) 0.001))
  ((iterative-improve good-enough? improve) x))

(define (square x) (* x x))

; Point

(define (make-point x y) (cons x y))
(define x-point car)
(define y-point cdr)

; Segment

(define (make-segment a b) (cons a b))
(define start-segment car)
(define end-segment cdr)

(define (projection-selector axis-selector)
  (lambda (segment)
    (abs (- (axis-selector (start-segment segment))
            (axis-selector (end-segment segment))))))
(define x-projection (projection-selector x-point))
(define y-projection (projection-selector y-point))

(define (length segment)
  (sqrt (+ (square (x-projection segment))
           (square (y-projection segment)))))

; Rectangle

(define (make-rectangle base-segment height-segment)
  (cons base-segment height-segment))

(define base-segment car)
(define height-segment cdr)

(define (side-length side)
  (lambda (rectangle) (length (side rectangle))))
(define width (side-length base-segment))
(define height (side-length height-segment))

(define (area r)
  (* (width r) (height r)))

(define (perimeter r)
  (* 2 (+ (width r) (height r))))

; Test

(define a (make-point 1.0 1.0))
(define b (make-point 1.0 3.0))
(define c (make-point 4.0 3.0))
(define ab (make-segment a b))
(define bc (make-segment b c))
(define abcd (make-rectangle ab bc))

(display (area abcd))
(newline)
(display (perimeter abcd))
(newline)
