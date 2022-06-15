#lang sicp

(define constant? number?)

(define variable? symbol?)

(define same-variable? eq?)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? exp)
  (and (pair? exp) (memq '+ exp)))

(define (addend exp)
  (define (compound-addend exp)
    (if (eq? (car exp) '+)
      nil
      (cons (car exp) (compound-addend (cdr exp)))))
  (if (eq? (cadr exp) '+)
      (car exp)
      (compound-addend exp)))

(define (augend exp)
  (let ((rest (memq '+ exp)))
    (if (null? (cddr rest))
        (cadr rest)
        (cdr rest))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? exp)
  (and (pair? exp) (not (memq '+ exp))))

(define multiplier car)
(define multiplicand caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; Solution

(define (deriv exp var)
  (cond ((constant? exp) 0)
        ((variable? exp) 
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else
          (error "неизвестный тип выражения -- DERIV" exp))))

(display (deriv '(x + 3 * (x + y + 2)) 'x))
(newline)
