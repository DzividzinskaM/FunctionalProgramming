#lang racket

(require "pretty-table.rkt")
(require "distinct.rkt")
(provide select)

(define listColumn null)
(define tableName null)
(define table (make-hash))
(define header null)

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

(define (GetCommandDistinct input)
  (set! listColumn (getColumn input 16)))


(define (select input InTable name)
  (display InTable)
  (display "\n")
  (set! tableName (getTableName input))
  (checkExistTable name InTable)
  (getHeader)
  (cond
    ((equal? (substring input 7 15) "distinct" ) (getColumn input 16))
    (#t   (getColumn input 6)))
  (checkColumn)
  (display InTable)
  (display "\n")
  (getSelectTable)
   (display InTable)
  (display "\n")
  (cond
    ((equal? (substring input 7 15) "distinct" ) (distinct table header)))
  (PrettyTableOutput table listColumn)

  ;(hash-clear! table)
  ;(set! header '())
  )


