#lang sicp

(#%require rackunit)

(define (fib3-recursive n)
  (if (< n 3)
      n
      (+ (fib3-recursive (- n 1))
         (fib3-recursive (- n 2))
         (fib3-recursive (- n 3)))))

(define (fib3 n)
  (define (fib3-iter a b c count)
    (if (= count -1)
        c
        (fib3-iter b
                   c
                   (+ a b c)
                   (- count 1))))
  (if (< n 3)
      n
      (fib3-iter 0 1 2 (- n 3))))

(check-equal? (fib3-recursive 6) 20)
(check-equal? (fib3 6) 20)
