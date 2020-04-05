#lang racket


(provide PrettyTableOutput)

(define listInList '())
(define count 0)
(define index 0)

(define (PrettyTableOutput table header)
  (set! count (length (hash-ref table (first header))))
  (for ([i count])
    (printRow header index table)
    (display "\n")
    (set! index (+ index 1)))
 )


(define (max-field-width header) (quotient 160 (length header)))


  
(define (minWidthForField lst)
  (first (sort (map (lambda (l)
       (append (string-length l)))
         lst) >)))

(define (minWidth lst header)
  (cond
    ((>(minWidthForField lst) (max-field-width header)) (max-field-width header))
        (#t (minWidthForField lst))))




(define (printRow header index table)
   (for ([i header])
     (display (string-append "|" (~a (list-ref (hash-ref table i) index)
                                     #:max-width (max-field-width header)
                                     #:min-width (minWidth (hash-ref table i) header)
                                     #:limit-marker "..."
                                     #:align 'center)))))
                                

