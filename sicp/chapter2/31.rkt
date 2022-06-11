#lang sicp

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Solution

(define (tree-map proc tree)
  (map (lambda (tree)
         (if (pair? tree)
             (tree-map proc tree)
             (proc tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define x (list 1 (list (list 2 3) (list 4 5))))

(display (square-tree x))
(newline)
