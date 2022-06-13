#lang sicp

(#%require sicp-pict)

(define (split transform1 transform2)
  (define (split-internal painter n)
    (if (= n 0)
        painter
        (transform1
          painter
          (let ((smaller (split-internal painter (- n 1))))
            (transform2 smaller smaller)))))
  split-internal)


(define right-split (split beside below))
(define up-split (split below beside))

(paint (up-split einstein 4))
(paint (right-split einstein 4))
