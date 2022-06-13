#lang sicp

(define (filter predicate items)
  (if (null? items)
      nil
      (let ((first (car items))
            (rest (filter predicate (cdr items))))
        (if (predicate first)
            (cons first rest)
            rest))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc sequence)
  (accumulate append
              nil
              (map proc sequence)))

(define (enumerate-interval low high)
  (if (< high low)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; Solution

(define empty-board nil)

;;; If items is (a1 a2 a3 ... an), then
;;; (enumerate items) is a list of pairs
;;; ((a1 . 1) (a2 . 2) ... (an . n))
(define (enumerate items)
  (define (enumerate-internal items i)
    (if (null? items)
        nil
        (cons (cons (car items) i)
              (enumerate-internal (cdr items) (+ i 1)))))
  (enumerate-internal items 1))

(define (some? predicate items)
  (define (predicate-indexed pair)
    (predicate (car pair) (cdr pair)))

  (define (some-indexed items-indexed)
    (cond ((null? items-indexed) false)
          ((predicate-indexed (car items-indexed)) true)
          (else (some-indexed (cdr items-indexed)))))
  
  (some-indexed (enumerate items)))

(define (none? predicate items)
  (not (some? predicate items)))

;;; Checks that queen at cell (x1 y1)
;;; beats queen at cell (x2 y2), given that x1 != x2
(define (beats? x1 y1 x2 y2)
  (or (= y1 y2)
      (= (- x1 y1) (- x2 y2))
      (= (+ x1 y1) (+ x2 y2))))

;;; positions is a list of integers that correspond
;;; that correspond to positions of queens at first
;;; k columns in reverse order
(define (safe? k positions)
  (none? (lambda (y i)
           (beats? (- k i) y k (car positions)))
         (cdr positions)))

(define adjoin-position cons)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols 8))

(display (queens 8))
(newline)
