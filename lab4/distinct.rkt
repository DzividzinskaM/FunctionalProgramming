#lang racket

(provide distinct)

(define ListRows null)


(define (distinct table header)
 (set! ListRows (append ListRows (hash->list table)))
  (set! ListRows (transpose ListRows))
  (set! ListRows (remove-duplicates ListRows))
  (set! ListRows (transpose ListRows))
  (set! table (returnToTableByColumn table))
  (set! ListRows '()))


(define (transpose lst)
  (apply map list lst))

(define (returnToTableByColumn table)
  (hash-clear! table)
  (map (lambda (l)
         (hash-set! table (first l) l))
       ListRows))
   
  


#|(define (CreateTableByRows table header)
  (for ([i header])
    (display (hash->list (hash-ref table i)))))|#




  
#|(define (CreateTableByRows table header)
  (set! index 0)
  (set! count (length (hash-ref table (first header))))
  (set! ListRows (for ([i count])
    (for ([j header])
      (string-append (list-ref (hash-ref table j) index)))
    (set! index (+ index 1))))
 )|#