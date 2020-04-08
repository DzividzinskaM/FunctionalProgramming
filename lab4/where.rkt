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


(define (GetColumnAndValue conditionLine separator)
  (set! column (first (string-split conditionLine separator)))
  (CheckColumn)
  (set! value (second (string-split conditionLine separator)))
  (set! index (index-of (first ListRows) column)))



(define (ConditionMoreEqual condition table)
  (GetColumnAndValue condition ">=")
  (cond
    ((not (string->number value)) (println "Value must to be number"))
    (#t (set! value (string->number value))))
  (set! ListRows (cddr ListRows))
  (map (lambda (l)
         (cond
           ((not (>= (string->number (list-ref l index)) value)) (set! ListRows (remove l ListRows)))
           )) 
        ListRows)
  (set! ListRows (append (list (reverse header)) ListRows)))



(define (ConditionEqual condition table)
  (GetColumnAndValue condition "=")
  (map (lambda (l)
         (cond
           ((equal? (list-ref l index) value) #t)
           (#t (set! ListRows (remove l ListRows)))))
       ListRows)
  (set! ListRows (append (list (reverse header)) ListRows)))
  

(define (getCondition condition table)
  (cond
    ((string-contains? condition ">=") (ConditionMoreEqual condition table))
    ((string-contains? condition "=") (ConditionEqual condition table))
    (#t (display "Operator don't find\n"))))


  
         

(define (where condition table)
  (set! ListRows null)
  (set! ListRows (append ListRows (hash->list table)))
  (set! ListRows (transpose ListRows))
  (set! header (hash-keys table)) 
  (getCondition condition table)
  (set! ListRows (transpose ListRows))
  (set! table (returnToTableByColumn table)))