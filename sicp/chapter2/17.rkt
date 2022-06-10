#lang sicp

(define (last-pair items)
  (let ((next (cdr items)))
    (if (null? next)
        items
        (last-pair next))))

(display (last-pair (list 1 2 3 4)))
(newline)
