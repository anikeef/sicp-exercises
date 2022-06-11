#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))
(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))
(define mobile? pair?)

(define (total-weight m)
  (if (mobile? m)
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))
      m))

(define (balanced? m)
  (define (torque b)
    (* (total-weight (branch-structure b))
       (branch-length b)))

  (= (torque (left-branch m))
     (torque (right-branch m))))

(define m1 (make-mobile (make-branch 1 2)
                        (make-branch 3 4)))

(define m2 (make-mobile (make-branch 5 6)
                        (make-branch 7 m1)))

(define m3 (make-mobile (make-branch 5 6)
                        (make-branch 5 m1)))

(display (total-weight m2)) ; 12
(newline)
(display (balanced? m2)) ; false
(newline)
(display (balanced? m3)) ; true
(newline)
