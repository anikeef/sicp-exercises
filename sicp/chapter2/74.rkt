#lang sicp

(define (get-record department name)
  (attach-tag department
              ((get 'get-record department) name)))

;;; Files in different departments should look like this:

;;; (define (install-dep1)
;;;   (define (get-record name) ...)
;;;   (define (get-salary record) ...)
;;;   ...
;;;   (put 'get-record 'dep1-file get-record)
;;;   (put 'get-salary 'dep1-record get-salary))

(define (get-salary record)
  (apply-generic 'get-salary record))

(define (find-employee-record name departments)
  (if (null? departments)
      false
      (let ((record get-record (car departments)))
        (if record
            record
            (find-employee-record name (cdr departments))))))
