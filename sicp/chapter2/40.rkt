#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flat-map proc sequence)
  (accumulate append
              nil
              (map proc sequence)))

(define (prime? n)
  (define (square x) (* x x))

  (define (divides? a b)
    (= (remainder b a) 0))

  (define (next a)
    (if (= a 2)
        3
        (+ a 2)))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (define (smallest-divisor n)
    (find-divisor n 2))

  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate items)
  (if (null? items)
      nil
      (let ((first (car items))
            (rest (filter predicate (cdr items))))
        (if (predicate first)
            (cons first rest)
            rest))))

;;; ; Solution

(define (enumerate-interval low high)
  (if (< high low)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(display (prime-sum-pairs 6))
(newline)
