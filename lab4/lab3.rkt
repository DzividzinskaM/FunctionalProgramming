#lang racket

(require "select.rkt")
(require "pretty-table.rkt")

#|
load("C:/Users/Smart/Desktop/2course/fp/lab3/plenary_register_mps-skl9.tsv")
select distinct date_agenda, id_event from plenary_register_mps-skl9
|#

#|load("C:/Users/Smart/Desktop/2course/fp/lab3/mp-posts_full.csv")|#

(define command null)
(define loadFile null)
(define type null)
(define input null)
(define index 0)
(define FileList null)
(define header null)
(define tableByColumn (make-hash))
(define tableName "")


(define (getTableName)
  (set! tableName
   (first (string-split (last (string-split loadFile "/")) "."))))


(define (CreateTableByColumn table)
  (set! index 0)
  (for ([h header])
    (hash-set! tableByColumn h (map (lambda (lst)
                                      (append (list-ref lst index)))
                                     FileList))
    (set! index (+ index 1))))


(define (listFromFile separator)
(set! FileList (map (lambda (lst)
       (string-split lst separator))
        (file->lines loadFile)))
  (set! header (first FileList)))


(define (ReadFile)
  (cond
    ((equal? type "csv") (listFromFile ","))
    ((equal? type "tsv") (listFromFile "\t"))
    (#t (println "error"))))


(define (GetType)
  (set! type (second (string-split loadFile "."))))


(define (CheckFile)
  (cond
    ((file-exists? (second (string-split input "\""))) (set! loadFile (second (string-split input "\""))))
    (#t (println "File don't find"))))


(define (load)
  (hash-clear! tableByColumn)
  (CheckFile)
  (GetType)
  (ReadFile)
  (CreateTableByColumn tableByColumn)
  (getTableName))
  


(define (GetCommand)
  (cond
    ((equal? input "") (println "String is empty"))
    ((equal? (substring input 0 4) "exit") (set! command "exit"))
    ((equal? (substring input 0 4) "load") (set! command "load"))
    ((equal? (string-downcase (substring input 0 6)) "select") (set! command "select"))
    (#t (println "Command don't find"))))



(define (run)
  (set! input (read-line))
  (GetCommand)
  (cond
    ((equal? command "load") (load))
    ((equal? command "select")  (select input tableByColumn tableName))
    (#t (println "error")))
  (cond
    ((equal? command "exit") (display "Program finish work"))
    (#t (run))))


(run)








