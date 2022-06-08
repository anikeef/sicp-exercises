#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (define (normalize a b)
    (if (< b 0)
        (cons (- a) (- b))
        (cons a b)))
  (let ((g (gcd (abs a) (abs b))))
    (normalize (/ a g)
               (/ b g))))

(define numer cat)
(define denom cdr)

(define (mul-rat a b)
  (make-rat (* (numer a) (number b))
            (* (denom a) (denom b))))
