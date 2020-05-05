#lang racket

(require "pretty-table.rkt")
(require "distinct.rkt")
(require "where.rkt")
(require "orderby.rkt")
(require "aggregateFunction.rkt")
(require "join.rkt")

(provide select)

(define listColumn null)
(define tableName null)
(define table (make-hash))
(define header null)
(define selectLine null)

(define (getColumn input from)
  (set! listColumn
  (map (lambda (lst)
         (string-trim lst))
       (string-split (first (string-split (substring input from) "from")) ","))))



(define (checkColumn)
  (map (lambda (lst)
         (cond
           ((equal? lst "*") (set! listColumn (reverse header)))
           ((member lst header) #t)
           (#t (display (string-append "column " lst " not found")))))
         listColumn))



(define (getTableName input)
  (string-trim (last (string-split input "from"))))



(define (checkExistTable name InTable)
  (cond
    ((equal? name tableName) (set! table (hash-copy InTable)))
    (#t (display (string-append "Table" tableName " don't load yet")))))



(define (getHeader)
  (set! header
        (map (lambda (lst)
               (string-normalize-spaces lst))
        	(hash-keys table))))

(define (getSelectTable)
  (map (lambda (h)
         (cond
           ((member h listColumn) #t)
           (#t (hash-remove! table h))))
       header))


(define (doSelect input InTable name selectLine)
   (cond
       ((string-contains? input "where") (where (string-normalize-spaces (second (string-split input "where"))) table)))       
  (getHeader)  
  (cond
    ((string-contains? selectLine "distinct") (getColumn selectLine 16))
    (#t (getColumn selectLine 6)))
  (cond
    ((string-contains? input "order by") (orderby table (second (string-split input "order by")))))
  (checkColumn)
  (getSelectTable)
  ;)
  (cond
    ((string-contains? selectLine "distinct") (distinct table header)))
  )



(define aggregateFunctionLst '("count" "avg" "sum" "min"))
(define aggFunc null)

(define ll null)
(define table1 (make-hash))
(define table2 (make-hash))


(define (doSelectFor2 input InTable1 name1 InTable2 name2)
  
  (set! ll (string-append "select * from " name2))
  (map (lambda (l)
               (set! ll (string-append ll " " l)))
       (cdr (string-split (last (string-split input (string-append "=" name2 "."))) " ")))

                                                                           
  (doSelectFor1 (first (string-split input joinCondition)) InTable1 name1)
  (doSelectFor1 ll InTable2 name2)
  
  (cond
    ((string-contains? input joinCondition) (join table1 table2 name1 name2 (string-append (first (cdr (string-split (second (string-split input joinCondition)) " ")))
                                                                                               " " (second (cdr (string-split (second (string-split input joinCondition)) " ")))) joinCondition))) 
  )



(define (doSelectFor1 input InTable name)
  (set! tableName name)
  (cond
     ((string-contains? input "order by") (set! selectLine (first (string-split input "order by"))))
     (#t (set! selectLine input)))
  (cond
    ((string-contains? input "where")  (set! selectLine (first (string-split input "where")))))
  (checkExistTable name InTable)
  
  (map (lambda (l)
           (cond
             ((string-contains? input l) (set! aggFunc l))))
          aggregateFunctionLst)
  (cond
    ((null? aggFunc) (doSelect input InTable name selectLine))
    (#t (aggregateFunction table selectLine aggFunc)))
  (cond
    ((hash-empty? table1) (set! table1 (hash-copy table)))
    (#t (set! table2 (hash-copy table)))))



(define joinCondition "")
(define joinLst '("inner join" "full outer join" "right join"))
  
(define (select input InTable1 name1 InTable2 name2)
  (map (lambda (l)
    (cond
    ((string-contains? input l) (set! joinCondition l)))
   (cond
    ((string-contains? input l) (doSelectFor2 input InTable1 name1 InTable2 name2))
   (#t
    (cond
      ((string-contains? input name1) (doSelectFor1 input InTable1 name1))
      ((string-contains? input name2)(doSelectFor1 input InTable2 name2))))))
       joinLst)
   )

 
  


