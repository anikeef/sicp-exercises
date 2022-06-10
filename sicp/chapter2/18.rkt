#lang sicp

(define (reverse items)
  (define (reverse-iter result postfix)
    (if (null? postfix)
        result
        (reverse-iter (cons (car postfix) result)
                      (cdr postfix))))
  (reverse-iter nil items))

(display (reverse (list 1 2 3 4)))
(newline)
