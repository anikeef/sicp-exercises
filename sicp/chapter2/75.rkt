#lang sicp

(define (make-from-mag-ang magnitude angle)
  (define (dispatch op)
    (cond ((= op 'real-part) (* (cos angle) magnitude))
          ((= op 'imag-part) (* (sin angle) magnitude))
          ((= op 'magnitude) magnitude)
          ((= op 'angle) angle)))
  dispatch)
