#lang racket

#| TASK 1 |#

(define (f1 l n)
  (cond ((equal? n 1)(cons n l))
        ((even? n)(cons n (cons '- (f1 l (- n 1)))))
        ((not (even? n)) (cons n (cons '+ (f1 l (- n 1)))))))

(define (t1 n)
  (reverse (f1 '() n)))

(t1 10)



