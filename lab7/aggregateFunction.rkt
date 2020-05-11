#lang racket

(provide aggregateFunction)

(define value null)
(define header '())
(define valueLst '())

(define (getHeader table)
  (set! header (hash-keys table)))

(define (checkValue)
  (cond
    ((member value header) #t)
    (#t (println "Value isn't correct"))))

(define (getValue line)
  (set! value (second (string-split (first (string-split line ")")) "("))))

(define (getList table)
  (set! valueLst (cdr (hash-ref table value))))



(define count 0)
(define sum 0)
(define outputValue 0)

(define (sumFun)
  (set! sum 0)
  (map (lambda (l)
              (set! sum (+ sum (string->number l))))
       valueLst)
  (set! outputValue sum))

(define (countFun)
  (set! count 0)
  (set! count (length valueLst))
   (set! outputValue count))

(define (avgFun)
  (sumFun)
  (countFun)
  (set! outputValue (/ sum count)))

(define (minFun)
  (set! outputValue (apply min (map string->number valueLst))))

(define (doFunction f out)
  (cond
    ((equal? f "count") (countFun))
    ((equal? f "sum") (sumFun))
    ((equal? f "avg") (avgFun))
    ((equal? f "min") (minFun))
    )
  (set! out outputValue))
;  (println outputValue))
  


(define (aggregateFunction table line f out)
  (println line)
 (getHeader table)
 (getValue line)
  (checkValue)
  (getList table)
  (doFunction f out))
  