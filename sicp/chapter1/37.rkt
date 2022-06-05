#lang sicp

(define (cont-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n 1)
         (+ (d 1)
            (cont-frac (lambda (i) (n (+ i 1)))
                       (lambda (i) (d (+ i 1)))
                       (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter p result)
    (if (= p 0)
        result
        (iter (- p 1)
              (/ (n p)
                 (+ (d p) result)))))
  (iter k 0))

(define (phi k)
  (/ 1
     (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     k)))

(display (phi 10))
