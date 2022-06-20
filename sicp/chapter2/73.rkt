#lang sicp

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define addend cadr)
(define augend caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define multiplier cadr)
(define multiplicand caddr)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define base cadr)
(define exponent caddr)

; Solution

(define (install-sum-derivative)
  (define (deriv-sum exp)
    (make-sum (deriv (addend exp) (augend exp))))

  (put 'deriv '+ deriv-sum))

(define (install-product-derivative)
  (define (deriv-product exp)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  
  (put 'deriv '* deriv-sum))

(define (install-exp-derivative)
  (define (deriv-exponentiation exp)
    (if (= (exponent exp) 0)
              0
              (make-product
                (exponent exp)
                (make-product (make-exponentiation
                                (base exp)
                                (- (exponent exp) 1))
                              (deriv (base exp) var)))))

  (put 'deriv '** deriv-exponentiation))