#lang racket

#| TASK 1 |#
((lambda (a b c)
   (list (car a) (car b) (car c)))
 '(H G (U J) (T R)) '(2 1 (+ 4 5)) '(TYPE CHAR REAL (H G)))


#| TASK 2 |#
(define (f a b c)
  (list (caddr a) (caddr b) (caddr c)))
(f '(TYPE CHAR REAL (H G)) '(2 1 (+ 4 5)) '(TYPE CHAR REAL (H G)))



#| TASK 3|#
(define (union x y)
  (cond ((null? y) x)
        ((member (car y) x)
         (union x (cdr y)))
        (#t (union (cons (car y) x) (cdr y)))))

(define (intersection x y)
  (cond ((null? x) null)
        ((member (car x) y)
         (cons (car x) (intersection (cdr x) y)))
        (#t (intersection (cdr x) y))))

(define (complement x y)
  (cond ((null? x) null)
        ((member (car x) y) (complement (cdr x) y))
        (#t (cons (car x) (complement (cdr x) y)))))

(define (f1 a b)
  (union (intersection a b)
         (complement a b)))

(f1 '(H G (U J) (T R)) '(2 1 (+ 4 5)))