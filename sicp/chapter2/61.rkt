#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(display (adjoin-set 4 (list 1 2 3 5 6 7)))
(newline)
