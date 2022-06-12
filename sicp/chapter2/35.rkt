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

; Solution

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       tree)))

(display (count-leaves (list 1 (list 2 3 (list 4) 5))))
(newline)
