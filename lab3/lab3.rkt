#lang racket
#|load("C:/Users/Smart/Desktop/2course/fp/lab3/plenary_register_mps-skl9(1).tsv")|#
#|load("C:/Users/Smart/Desktop/2course/fp/lab3/mp-assistants(1).csv")|#


(define command null)
(define loadFile null)
(define type null)
(define input null)

(define (table l)
  (map (lambda (lst)
         (~a "|" lst  #:min-width 20
             #:max-width 20
             #:limit-marker "..."))
       l))

(define (ReadCsv)
  (map (lambda (l)
       (table l))
     (map (lambda (l)
        (string-split l ","))
    (file->lines loadFile))))


(define (ReadTsv)
  (map (lambda (l)
       (table l))
     (map (lambda (l)
        (string-split l "\t"))
    (file->lines loadFile))))


(define (ReadFile)
  (cond
    ((equal? type "csv") (ReadCsv))
    ((equal? type "tsv") (ReadTsv))
    (#t (println "error"))))


(define (GetType)
  (set! type (second (string-split loadFile "."))))


(define (CheckFile)
  (cond
    ((file-exists? (second (string-split input "\""))) (set! loadFile (second (string-split input "\""))))
    (#t (println "File don't find"))))



(define (load)
  (CheckFile)
  (GetType)
  (ReadFile))
  


(define (GetCommand)
  (cond
    ((equal? input "") (println "String is empty"))
    ((equal? (substring input 0 4) "load") (set! command "load"))
    (#t (println "Command don't find"))))



(define (run)
  (set! input (read-line))
  (GetCommand)
  (cond
    ((equal? command "load") (load))
    (#t (println "error"))))

(run)