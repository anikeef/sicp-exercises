#lang sicp

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Solution

(define (square-tree-1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (tree)
         (if (pair? tree)
             (square-tree-2 tree)
             (square tree)))
       tree))

(define x (list 1 (list (list 2 3) (list 4 5))))

(display (square-tree-1 x))
(newline)
(display (square-tree-2 x))
(newline)
