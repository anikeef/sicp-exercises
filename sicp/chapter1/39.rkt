#lang sicp

(define (cont-frac n d k)
  (define (iter p result)
    (if (= p 0)
        result
        (iter (- p 1)
              (/ (n p)
                 (+ (d p) result)))))
  (iter k 0))

(define (square x) (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
              (lambda (i) (- (* 2 i) 1))
              k))

(display (tan-cf 3.1415 100))
(newline)
