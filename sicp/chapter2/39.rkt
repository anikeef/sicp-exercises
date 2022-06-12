#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (append seq1 seq2)
  (fold-right cons seq2 seq1))

; Solution

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x))
             nil
             sequence))

(define x (list 1 2 3 4 5))

(display (reverse-right x))
(newline)
(display (reverse-left x))
(newline)
