#lang sicp

; Leaf

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; Tree

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Encoding

(define (symbol-in-tree? symbol tree)
  (if (memq symbol (symbols tree)) true false))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((not (symbol-in-tree? symbol tree))
          (error "Symbol is not present in tree: " symbol))
        ((symbol-in-tree? symbol (left-branch tree))
          (cons 0 (encode-symbol symbol (left-branch tree))))
        (else (cons 1 (encode-symbol symbol (right-branch tree))))))

(define (encode message tree)
  (if (null? message)
  '()
  (append (encode-symbol (car message) tree)
          (encode (cdr message) tree))))

; Tree generation

(define (successive-merge nodes)
  (if (null? (cdr nodes))
      (car nodes)
      (successive-merge
        (adjoin-set
          (make-code-tree (car nodes)
                          (cadr nodes))
          (cddr nodes)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Solution

(define tree
  (generate-huffman-tree
    '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))))

(define message
  '(get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom))

(display "Huffman length: ")
(display (length (encode message tree)))
(newline)
(display "Fixed length: ")
(display (* 3 (length message)))
(newline)
