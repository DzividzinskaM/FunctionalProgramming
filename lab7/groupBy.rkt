#lang racket

(require "pretty-table.rkt")

(provide groupBy)

(define groupValue null)
(define result (make-hash))
(define resultRow null)
(define local (make-hash))
(define localLst null)
(define localLstOfAgg null)
(define header null)
(define aggValue null)
(define aggFun null)

;(define indexLst null)
;(define localLst null)

(define aggFunctions '("count" "sum" "min" "avg"))

(define (transpose lst)
  (apply map list lst))


(define (returnToTableByColumn result)
  (hash-clear! result)
  (map (lambda (l)
         (hash-set! result (first l) l))
       resultRow))


(define (getValues line table)
  (set! groupValue (second (string-split line "group by ")))
  (map (lambda (l)
         (cond
           ((string-contains? line l) (set! aggFun l))))
       aggFunctions)
  (set! aggValue (substring (first (string-split (second (string-split line aggFun)) ")")) 1))
  (set! localLst (hash-ref table groupValue))
  (set! localLstOfAgg (hash-ref table aggValue))
  (hash-set! local groupValue (hash-ref table groupValue))
  (hash-set! local aggValue (hash-ref table aggValue)))

(define lstForAgg '())
(define out null)

(define (count l)
  (set! out (length l)))

(define (doAggFun l)
  (count l))

(define p null)

(define (doGroupBy)
  (map (lambda (l)
         (set! lstForAgg null)
             (map (lambda (k)
                    (set! lstForAgg (append (list (list-ref localLstOfAgg k)) lstForAgg))
                    )
                   (indexes-of localLst l))
       
         (doAggFun lstForAgg)
         (set! p (list l (number->string out)))
         (set! resultRow (append resultRow (list p)))
              )
       localLst)
  (set! resultRow (remove-duplicates resultRow))
  (set! resultRow (cdr resultRow))
  (set! header (list  groupValue  aggValue))
  (set! resultRow (append (list header) resultRow))
  )

(define havingCond null)
(define havingVal null)

(define (doHaving line)
  (set! havingCond (second (string-split (second (string-split line "having")) " ")))
  (set! havingVal (string->number (third (string-split (second (string-split line "having")) " "))))
  (map (lambda (l)
         (set! lstForAgg null)
             (map (lambda (k)
                    (set! lstForAgg (append (list (list-ref localLstOfAgg k)) lstForAgg))
                    )
                   (indexes-of localLst l))
       
         (doAggFun lstForAgg)
         (set! p (list l (number->string out)))
         (cond
           ((equal? havingCond "=") (cond
                                      ((equal? out havingVal) (set! resultRow (append resultRow (list p))))))
           ((equal? havingCond ">") (cond
                                      ((> out havingVal) (set! resultRow (append resultRow (list p))))))
            ((equal? havingCond "<") (cond
                                      ((< out havingVal) (set! resultRow (append resultRow (list p))))))
            )       
              )
       localLst)
  (set! resultRow (remove-duplicates resultRow))
  (set! resultRow (cdr resultRow))
  (set! header (list  groupValue  aggValue))
  (set! resultRow (append (list header) resultRow))
  )
  
         
         
(define selectLine "")       
        

(define (groupBy line table)
   (cond
    ((string-contains? line "having") (set! selectLine (first (string-split line " having")))))
  (getValues selectLine table)
  (cond
    ((string-contains? line "having") (doHaving line))
    (#t (doGroupBy)))  
  (set! header (car resultRow))
  (set! resultRow (transpose resultRow))
  (returnToTableByColumn result)
  (PrettyTableOutput result header))

  