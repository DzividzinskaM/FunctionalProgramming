#lang racket

(require "pretty-table.rkt")
(require "distinct.rkt")
(require "where.rkt")
(require "orderby.rkt")

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


;(define getSelectLine


(define (select input InTable name)
  (cond
     ((string-contains? input "order by") (set! selectLine (first (string-split input "order by"))))
     (#t (set! selectLine input)))
  (cond
    ((string-contains? input "where")  (set! selectLine (first (string-split input "where")))))
  ;(#t (set! selectLine input)))
 
  (set! tableName (getTableName selectLine))
  (checkExistTable name InTable)
  (cond
       ((string-contains? input "where") (where (string-normalize-spaces (second (string-split input "where"))) table)))       
  (getHeader)  
  (cond
    ((string-contains? selectLine "distinct") (getColumn selectLine 16))
    (#t   (getColumn selectLine 6)))
  (cond
    ((string-contains? input "order by") (orderby table (second (string-split input "order by")))))
  (checkColumn)
  (getSelectTable)
  (cond
    ((string-contains? selectLine "distinct") (distinct table header)))
  (PrettyTableOutput table listColumn)
  )


