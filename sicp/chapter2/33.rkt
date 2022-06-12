#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map proc sequence)
  (accumulate (lambda (x y)
                (cons (proc x) y))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(display (map (lambda (x) (* 2 x))
              (list 1 2 3 4 5)))
(newline)

(display (append (list 1 2 3 4 5)
                 (list 6 7 8 9 10)))
(newline)

(display (length (list 1 2 3 4 5)))
(newline)
