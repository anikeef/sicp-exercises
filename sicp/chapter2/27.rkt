#lang sicp

(define (fringe items)
  (define (fringe-iter items answer)
    (cond ((null? items) answer)
          ((pair? items)
            (fringe-iter (car items)
                         (fringe-iter (cdr items) answer)))
          (else (cons items answer))))
  (fringe-iter items nil))

(define x (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)
(display (fringe (list x x)))
(newline)
