#lang racket

#| TASK 3.2 |#
(define (quick-sort inlist)
  (cond ((null? inlist) null)
        (#t (append (quick-sort (l< (car inlist) (cdr inlist)))
                    (cons (car inlist) null)
                    (quick-sort (l>= (car inlist) (cdr inlist)))))))

(define (l< a b)
  (cond ((or (null? a) (null? b)) null)
        ((< (string-length a) (string-length (car b))) (l< a (cdr b)))
        (#t (cons (car b) (l< a(cdr b))))))

(define (l>= a b)
  (cond ((or (null? a) (null? b)) null)
        ((>= (string-length a) (string-length (car b))) (l>= a (cdr b)))
        (#t (cons (car b) (l>= a (cdr b))))))

(quick-sort (string-split "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. "))


