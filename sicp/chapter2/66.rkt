#lang sicp

(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry
                                  left-tree
                                  right-tree)
                        remaining-elts))))))))
  (car (partial-tree elements (length elements))))

; Solution

(define key-value-pair cons)
(define key car)

(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((equal? (key (entry tree)) given-key)
          (entry tree))
        ((< (key (entry tree)) given-key)
          (lookup given-key (right-branch tree)))
        (else (lookup given-key (left-branch tree)))))

(define s
  (list->tree
    (list (key-value-pair 1 'a)
          (key-value-pair 2 'b)
          (key-value-pair 4 'c)
          (key-value-pair 5 'd)
          (key-value-pair 7 'e)
          (key-value-pair 8 'f))))

(display (lookup 6 s))
(newline)
(display (lookup 2 s))
(newline)
