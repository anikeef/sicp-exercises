#lang sicp

(#%require sicp-pict)

(define (up-split painter n)
  (if (= n 0)
      painter
      (below painter
            (let ((smaller (up-split painter (- n 1))))
              (beside smaller smaller)))))

(paint (up-split einstein 4))
