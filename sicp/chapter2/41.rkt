#lang sicp

(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (filter predicate items)
  (if (null? items)
      nil
      (let ((first (car items))
            (rest (filter predicate (cdr items))))
        (if (predicate first)
            (cons first rest)
            rest))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flat-map proc sequence)
  (accumulate append
              nil
              (map proc sequence)))

(define (enumerate-interval low high)
  (if (< high low)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; Solution

(define (unique-triples n)
  (flat-map (lambda (i)
              (map (lambda (pair)
                     (append (list i) pair))
                   (unique-pairs (- i 1))))
            (enumerate-interval 1 n)))

(define (sum items)
  (accumulate + 0 items))

(define (sum-up-to? s items)
  (= s (sum items)))

(define (sum-triples n s)
  (filter (lambda (triple) (sum-up-to? s triple))
          (unique-triples n)))

(display (sum-triples 5 7))
(newline)
