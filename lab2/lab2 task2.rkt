#lang racket
#| TASK 2 |#

(define (quick-sort inlist)
  (cond ((null? inlist) null)
        (#t (append (quick-sort (l< (car inlist) (cdr inlist)))
                    (cons (car inlist) null)
                    (quick-sort (l>= (car inlist) (cdr inlist)))))))

(define (l< a b)
  (cond ((or (null? a) (null? b)) null)
        ((< a (car b))(l< a (cdr b)))
        (#t (cons (car b) (l< a(cdr b))))))

(define (l>= a b)
  (cond ((or (null? a) (null? b)) null)
        ((>= a (car b)) (l>= a (cdr b)))
        (#t (cons (car b) (l>= a (cdr b))))))
         

(quick-sort '(1 -2 3 -4 5 -6 7 -8))
