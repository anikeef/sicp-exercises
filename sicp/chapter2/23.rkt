#lang sicp

(define (for-each proc items)
  (define (next)
    (proc (car items))
    (for-each proc (cdr items)))

  (if (null? items) true (next)))

(for-each (lambda (x) (display x) (newline))
          (list 1 2 3 4 5))
