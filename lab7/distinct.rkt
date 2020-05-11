#lang racket

(provide distinct)

(define ListRows null)


(define (distinct table header)
 (set! ListRows (append ListRows (hash->list table)))
  (set! ListRows (transpose ListRows))
  (set! ListRows (remove-duplicates ListRows))
  (set! ListRows (transpose ListRows))
  (set! table (returnToTableByColumn table))
  (set! ListRows '()))


(define (transpose lst)
  (apply map list lst))

(define (returnToTableByColumn table)
  (hash-clear! table)
  (map (lambda (l)
         (hash-set! table (first l) l))
       ListRows))
   
 