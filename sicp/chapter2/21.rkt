#lang sicp

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Solution

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))
