#lang sicp

(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) false)
        (else (eq? a b))))

(display (equal? '(a (b c) d) '(a (b c) d)))
(newline)
(display (equal? '(a (b) d) '(a (b c) d)))
(newline)
