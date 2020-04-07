#lang racket

(provide where)

(define header null)
(define column null)
(define value null)
(define ListRows null)
(define index null)

(define (transpose lst)
  (apply map list lst))



(define (CheckColumn)
  (cond
    ((member column header) #t)
    (#t (display "Column don't find"))))


(define (returnToTableByColumn table)
  (hash-clear! table)
  (map (lambda (l)
         (hash-set! table (first l) l))
       ListRows))
               

(define (ConditionEqual condition table)
  (set! column (first (string-split condition "=")))
  (CheckColumn)
  (set! value (second (string-split condition "=")))
  (set! index (index-of (first ListRows) column))
  (map (lambda (l)
         (cond
           ((equal? (list-ref l index) value) #t)
           (#t (set! ListRows (remove l ListRows)))))
       ListRows)
  (set! ListRows (append (list (reverse header)) ListRows))
  (display ListRows)
  (display "\n"))
  

(define (getCondition condition table)
  (cond
    ((string-contains? condition "=") (ConditionEqual condition table))
    (#t (display "Operator don't find\n"))))


  
         

(define (where condition table)
  (set! ListRows (append ListRows (hash->list table)))
  (set! ListRows (transpose ListRows))
  (set! header (hash-keys table)) 
  (getCondition condition table)
  (set! ListRows (transpose ListRows))
  (set! table (returnToTableByColumn table)))