#lang sicp

(define (cont-frac n d k)
  (define (iter p result)
    (if (= p 0)
        result
        (iter (- p 1)
              (/ (n p)
                 (+ (d p) result)))))
  (iter k 0))

(define (e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (* (/ (+ i 1) 3) 2)
                      1))
                k)))

(display (e 100))
(newline)
