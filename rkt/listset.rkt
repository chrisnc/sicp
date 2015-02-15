; Example 2.3.3: Representing Sets, page 206

(define (element-of-set? x s)
  (cond ((null? s) false)
        ((equal? x (car s)) true)
        (else (element-of-set? x (cdr s)))))

(define (adjoin-set x s)
  (if (element-of-set? x s)
    s
    (cons x s)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Exercise 2.59, page 207
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
