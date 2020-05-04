#lang racket

(require "select.rkt")
(require "pretty-table.rkt")


(define command null)
(define loadFile null)
(define type null)
(define input null)
(define index 0)
(define FileList null)
(define header null)
(define tableByColumn (make-hash))
(define tableName "")

(define table1 (make-hash))
(define table2 (make-hash))
(define tableName1 "")
(define tableName2 "")


(define (getTableName)
  (set! tableName
   (first (string-split (last (string-split loadFile "/")) ".")))
  (cond
    ((equal? tableName1 "") (set! tableName1 tableName))
     (#t (set! tableName2 tableName))))


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
  (set! header null)
  (CheckFile)
  (GetType)
  (ReadFile)
  (CreateTableByColumn tableByColumn)
  (getTableName)
  (cond
    ((hash-empty? table1) (set! table1 (hash-copy tableByColumn)))
     (#t (set! table2 (hash-copy tableByColumn)))) 
  (println table1)
  (println table2))
  


(define (GetCommand)
  (cond
    ((equal? input "") (println "String is empty"))
    ((string-contains? input "exit") (set! command "exit"))
    ((string-contains? input "load") (set! command "load"))
    ((string-contains? input "select")(set! command "select"))
    (#t (println "Command don't find"))))



(define (run)
  (set! input (read-line))
  (GetCommand)
  (cond
    ((equal? command "load") (load))
    ((equal? command "select") (select input table1 tableName1 table2 tableName2))
    (#t (println "error")))
  (cond
    ((equal? command "exit") (display "Program finish work"))
    (#t (run))))


(run)








