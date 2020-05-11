#lang racket

(provide join)

(require "pretty-table.rkt")

(define joinCond1 "")
(define joinCond2 "")
(define header1 null)
(define header2 null)
(define ListRows1 null)
(define ListRows2 null)
(define ListRows3 null)
(define index1 null)
(define index2 null)

(define newTable (make-hash))


(define (transpose lst)
  (apply map list lst))

(define (returnToTableByColumn)
  (hash-clear! newTable)
  (map (lambda (l)
         (hash-set! newTable (first l) l))
       ListRows3))


(define (getJoinCond input name1 name2)
  (set! joinCond1 (last (string-split (last (string-split (first (string-split (last (string-split input "on")) "=")) name1)) ".")))
  (set! joinCond2 (last (string-split (last (string-split (last (string-split (last (string-split input "on")) "=")) name2)) "."))))


(define (getHeaders table1 table2)
  (set! header1 (hash-keys table1))
  (set! header2 (hash-keys table2)))


(define (checkCond)
  (cond
    ((not (member joinCond1 header1)) (println "error ij1"))
    ((not (member joinCond2 header2)) (println "error ij2"))))






(define helpList null)


(define (innerJoin)
  (set! index1 (index-of (car ListRows1) joinCond1))
  (set! index2 (index-of (car ListRows2) joinCond2))
  (map (lambda (t2)
         (map (lambda (t1)
                (cond
                  ((equal? (list-ref t1 index1) (list-ref t2 index2)) (set! ListRows3 (append ListRows3 (list(remove-duplicates (append t2 t1)))))))
                 (cond
                  ((equal? (list-ref t1 index1) (list-ref t2 index2))(set! ListRows1 (remove t1 ListRows1))))
                (cond
                  ((equal? (list-ref t1 index1) (list-ref t2 index2))(set! ListRows2 (remove t2 ListRows2)))))
              ListRows1))ListRows2)
  (set! ListRows3 (remove-duplicates ListRows3)))


(define (rightJoin)
  (set! index1 (index-of (car ListRows1) joinCond1))
  (set! index2 (index-of (car ListRows2) joinCond2))
  (innerJoin)
  (set! helpList (make-list (- (length (remove-duplicates (append header1 header2))) (length header2)) "-"))
  (map (lambda (l)
         (set! ListRows3 (append ListRows3 (list (append l helpList)))))
       ListRows2))
  

(define (fullOuterJoin)
  (set! index1 (index-of (car ListRows1) joinCond1))
  (set! index2 (index-of (car ListRows2) joinCond2))
  (rightJoin)
  (set! helpList (make-list (- (length (remove-duplicates (append header1 header2))) (length header1)) "-"))
  (map (lambda (l)
         (set! ListRows3 (append ListRows3  (list (append (list (list-ref l index1)) helpList (car l))))))
       ListRows1)
  (println ListRows3))




(define newHeader null)

(define (join table1 table2 name1 name2 input joinline)
  (getJoinCond input name1 name2)
  
  (getHeaders table1 table2)
  (checkCond)
  ;(deleteDuplicatesColumn)
  (set! ListRows1 (transpose (hash->list table1)))
  (set! ListRows2 (transpose (hash->list table2)))
  (cond
    ((equal? joinline "inner join") (innerJoin))
     ((equal? joinline "full outer join") (fullOuterJoin))
      ((equal? joinline "right join") (rightJoin)))
  (set! newHeader (car ListRows3))
  (set! ListRows3 (transpose ListRows3))
  (returnToTableByColumn)
  (PrettyTableOutput newTable newHeader)
  )