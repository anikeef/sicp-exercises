#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Solution

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(define x (list (list 1 2) (list 3 4) (list 5 6)))
(define y (list 7 8))
(define z (list (list 1 2) (list 3 4)))

(display (matrix-*-vector x y)) ; (23 53 83)
(newline)
(display (transpose x)) ; ((1 3 5) (2 4 6))
(newline)
(display (matrix-*-matrix x z))
(newline)
