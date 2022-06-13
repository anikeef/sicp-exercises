#lang sicp

(#%require sicp-pict)

(define make-frame list)
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
        (scale-vect (xcor-vect v)
                    (edge1-frame frame))
        (scale-vect (ycor-vect v)
                    (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line  ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

; Solution

(define v00 (make-vect 0 0))
(define v10 (make-vect 1 0))
(define v01 (make-vect 0 1))
(define v11 (make-vect 1 1))

(define outline
  (segments->painter (list (make-segment v00 v10)
                           (make-segment v10 v11)
                           (make-segment v11 v01)
                           (make-segment v01 v00))))
