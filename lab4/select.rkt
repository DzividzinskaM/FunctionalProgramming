#lang racket

(require "pretty-table.rkt")
(provide select)

(define listColumn null)
(define tableName null)
(define table (make-hash))
(define header null)

(define (getColumn input)
  (map (lambda (lst)
         (string-trim lst))
       (string-split (first (string-split (substring input 6) "from")) ",")))



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
    ((equal? name tableName) (set! table InTable))
    (#t (display (string-append "Table" tableName " don't load yet")))))



(define (getHeader)
  (set! header (hash-keys table)))

(define (getSelectTable)
  (map (lambda (h)
         (cond
           ((member h listColumn) #t)
           (#t (hash-remove! table h))))
       header))



(define (select input InTable name)
  (set! tableName (getTableName input))
  (checkExistTable name InTable)
  (getHeader)
  (set! listColumn (getColumn input))
  (checkColumn)
  (display "\n")
  (getSelectTable)
  (PrettyTableOutput table listColumn)
  )


