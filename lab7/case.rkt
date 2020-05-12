#lang racket

(require "pretty-table.rkt")

(provide case)

(define caseLst null)
(define listRows null)
(define caseCol null)
(define caseElse null)
(define caseColName null)
(define caseWhenLst null)
(define p null)
(define header null)
(define resultRow null)
(define resultTable (make-hash))

(define (transpose lst)
  (apply map list lst))


(define (returnToTableByColumn table)
  (hash-clear! table)
  (map (lambda (l)
         (hash-set! table (first l) l))
       resultRow))


(define (getValue line table)
  (set! listRows (transpose (hash->list table)))
  (set! caseCol (first (string-split (second (string-split line "when")) " ")))
  (set! caseLst (hash-ref table caseCol))
  (set! caseElse (first (string-split (second (string-split line "else '")) "'")))
  (set! caseColName (first (string-split (second (string-split line "as ")) " ")))
  (map (lambda (l)
         (set! p null)
       (set! p (append (list (second (string-split l " "))) (list (third (string-split l " "))) (list (fifth (string-split l " ")))))
       (set! caseWhenLst (append caseWhenLst (list p))) )
     (string-split (first (string-split (second (string-split line "case ")) " else")) "when"))
  )

(define localResult null)
(define res null)
(define (doCase)
  (map (lambda (l)
         (set! res null)
         (map (lambda (c)
                (cond
                  ((equal? (first c) "=") (cond
                                            ((equal? l (second c)) (set! res (third c)))))                     
                  ((equal? (first c) ">") (cond
                                            ((> (string->number l) (string->number (second c)))  (set! res (third c)))))         
                  ((equal? (first c) "<") (cond
                                            ((< (string->number l) (string->number (second c)))  (set! res (third c))))))
                
                )
              caseWhenLst)
         (cond
           ((null? res) (set! res caseElse)))
         (set! localResult (append localResult (list res))) 
         )
       (cdr caseLst))
  )


(define (getResult)
  (set! header (car listRows))
  (set! listRows (cddr listRows))
  (for ( [i listRows]
         [j localResult])
    (set! resultRow (append resultRow (list (append i (list j)))))
    )
  (set! resultRow (append (list (append header (list caseColName))) resultRow))
  (set! header (car resultRow))
  (set! resultRow (transpose resultRow))
  (returnToTableByColumn resultTable)
 ( PrettyTableOutput resultTable header)
    )
  

        

(define (case line table)
  (getValue line table)
  (doCase)
  (getResult)
  )