#lang sicp

(define (deep-reverse items)
  (define (reverse-iter result postfix)
    (if (null? postfix)
        result
        (reverse-iter (cons (deep-reverse (car postfix)) result)
                      (cdr postfix))))
  (if (pair? items)
      (reverse-iter nil items)
      items))

(define x (list (list 1 2) (list 3 4)))
(display (deep-reverse x))
(newline)
