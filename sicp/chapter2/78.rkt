#lang sicp

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Некорректные помеченные данные -- CONTENTS" datum))))
