#lang racket

(provide orderby)

(define columns null)
(define sortCond "")
(define header null)
(define sortIndex null)
(define localListRows null)
(define localListSort null)


(define (transpose lst)
  (apply map list lst))



(define (returnToTableByColumn table)
  (hash-clear! table)
  (map (lambda (l)
         (hash-set! table (first l) l))
       ListRows))



(define (getConditions line)
  (cond
    ((string-contains? line "desc") (set! sortCond "desc"))
    (#t (set! sortCond "asc"))))

    
(define (getColumns line)
  (set! columns (string-split line " "))
  (set! columns (reverse (cdr (reverse columns))))
    )
  
(define (getHeader table)
  (set! header (hash-keys table)))


(define (checkColumn)
  (map (lambda (l)
         (cond
           ((member l header) #t)
           (#t (println "error"))))
       columns))

  
(define ListRows '())

(define (getSortList sortKey table)
  (set! localListSort (hash-ref table sortKey)))


(define i null)

(define (doSort table)
  (map (lambda (column)
         (set! ListRows null)
         (set! localListRows (reverse localListRows))
         (getSortList column table)
         (set! localListSort (cdr localListSort))
         (cond
           ((equal? sortCond "asc")  (set! localListSort (sort localListSort string<?)))
           (#t (set! localListSort (sort localListSort string>?))))        
         (set! sortIndex (index-of  header column))
  

         (map (lambda (s)
                (map (lambda (k)
                       (cond
                         ((member s k) (set! i k))))
                     localListRows)
                (set! ListRows (append ListRows (list i)))
                (set! localListRows (remove i localListRows)))
              localListSort)
         (set! ListRows (append (list header) ListRows))
         (set! localListRows ListRows))
       columns))
  
  
  

(define (orderby table line)
  (set! localListRows (append localListRows (hash->list table)))
  (set! localListRows (transpose localListRows))
  
  (getConditions line)
  (getHeader table)
  (getColumns line)
  (checkColumn)
  (doSort table)

  (set! ListRows (transpose ListRows))
  (set! table (returnToTableByColumn table))
  (set! localListRows null)
  (set! ListRows null)
  (set! localListSort null)
)