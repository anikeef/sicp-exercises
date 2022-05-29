#lang sicp

; Recursive process
(define (plus1 a b)
  (if (= a 0)
      b
      (inc (plus1 (dec a) b))))

#|
(plus1 4 5)
(inc (plus1 3 5))
(inc (inc (plus1 2 5)))
(inc (inc (inc (plus1 1 5))))
(inc (inc (inc (inc (plus1 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
|#

; Iterative process
(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))

#|
(plus2 4 5)
(plus2 3 6)
(plus2 2 7)
(plus2 1 8)
(plus2 0 9)
9
|#
