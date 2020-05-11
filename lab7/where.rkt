#lang racket

(provide where)

(define header null)
(define column null)
(define value null)
(define ListRows null)
(define index null)

(define strList null)
(define addCond null)
(define addValue null)

(define localList null)



(define (transpose lst)
  (apply map list lst))



(define (CheckColumn)
  (cond
    ((member column header) #t)
    (#t (display "Column don't find"))))



(define (returnToTableByColumn table)
  (hash-clear! table)
  (map (lambda (l)
         (hash-set! table (first l) l))
       ListRows))


(define (GetColumnAndValue conditionLine separator)
  (set! column (first (string-split conditionLine separator)))
  (CheckColumn)
  (set! value (second (string-split conditionLine separator)))
  (set! index (index-of (first localList) column)))



(define (ConditionMoreEqual condition table)
  (GetColumnAndValue condition ">=")
  (cond
    ((not (string->number value)) (println "Value must to be number"))
    (#t (set! value (string->number value))))
  (set! localList (cddr localList))
  (map (lambda (l)
         (cond
           ((not (>= (string->number (list-ref l index)) value)) (set! localList (remove l localList)))
           )) 
       localList)
  (set! localList (append (list (reverse header)) localList)))


(define (ConditionNotEqual condition table)
  (GetColumnAndValue condition "<>")
  (set! localList (cddr localList))
  (map (lambda (l)
         (cond
           ((equal? (list-ref l index) value) (set! localList (remove l localList)))))
       localList)
   (set! localList (append (list (reverse header)) localList)))  
         

(define (ConditionEqual condition table)
  (GetColumnAndValue condition "=")
  (map (lambda (l)
         (cond
           ((equal? (list-ref l index) value) #t)
           (#t (set! localList (remove l localList)))))
       localList)
  (set! localList (append (list (reverse header)) localList)))
  

(define (getCondition condition table)
  
  (cond
    ((string-contains? condition "<>") (ConditionNotEqual condition table))
    ((string-contains? condition ">=") (ConditionMoreEqual condition table))
    ((string-contains? condition "=") (ConditionEqual condition table))))
    ;(#t (display "Operator don't find\n"))))

(define k 0)

(define (andCondition andCond table flagNot)
  (getCondition andCond table)
  (cond
    ((false? flagNot)
  (map (lambda (l)
        (cond
          ((false? (member l localList)) (set! ListRows (remove l ListRows)))))
       ListRows))
    (#t
       (map (lambda (l)
        (cond
          ((member l localList) (set! ListRows (remove l ListRows)))))
       ListRows)))
  
  (cond 
        ((empty? ListRows) (append header ListRows))))



(define (orCondition orCond table flagNot)
  (set! localList inputList)
  (getCondition orCond table)
  (cond
    ((false? flagNot)
  (map (lambda (l)
         (cond
           ((not (member l ListRows)) (set! ListRows (append ListRows (list l))))))
       localList))
    (#t
     (map (lambda (l)
         (cond
           ((false? (member l localList)) (set! ListRows (append ListRows (list l))))))
          inputList))))


(define (notCondition notCond table)
   (set! localList inputList)
   (getCondition notCond table)
   (map (lambda (l)
          (cond
            ((false? (not (member l localList))) (set! ListRows (remove l ListRows)))))
          ListRows)
  (set! ListRows (append (list (reverse header)) ListRows)))
  


(define listCondition '())

(define (moreConditions condition table)
  (set! listCondition (string-split condition))
  (cond
    ((equal? (first listCondition) "not") (notCondition (second listCondition) table)))
  (cond
    ((equal? (first listCondition) "not") (set! listCondition (cddr listCondition))))

  (set! localList ListRows)
  (set! k 0)
  (map (lambda (l)
         (set! k (+ k 1))
            (cond
              ((equal? l "and")
               (cond
                 ((equal? (list-ref listCondition k) "not") (andCondition (list-ref listCondition (+ k 1)) table #t))
                 (#t (andCondition (list-ref listCondition k) table #f))))
              ((equal? l "or")
                (cond
                 ((equal? (list-ref listCondition k) "not") (orCondition (list-ref listCondition (+ k 1)) table #t))
                 (#t (orCondition (list-ref listCondition k) table #f))))))           
       listCondition))
       
  
         


(define inputList null)

(define (where condition table)
  (set! ListRows null)
  (set! ListRows (append ListRows (hash->list table)))
  (set! ListRows (transpose ListRows))
  (set! inputList ListRows)
  (set! header (hash-keys table))
  

  (set! localList inputList)

  (getCondition (first (string-split condition)) table)
  (set! ListRows localList)
  (set! localList inputList)
  (cond
    ((> (length (string-split condition)) 1) (moreConditions  condition table)))
  (set! ListRows (transpose ListRows))
  (set! table (returnToTableByColumn table))
  )