#lang sicp

(#%require rackunit)

(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multiply a b)
  (define (multiply-iter a b c)
    (cond ((= b 0) c)
          ((even? b) (multiply-iter (double a)
                                    (halve b)
                                    c))
          (else (multiply-iter a
                               (- b 1)
                               (+ c a)))))
  (multiply-iter a b 0))

(check-equal? (multiply 7 12) 84)
