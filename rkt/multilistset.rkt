; Exercise 2.60, page 207

; this is unchanged from the regular set case
; O(n)
(define (element-of-set? x s)
  (cond ((null? s) false)
        ((equal? x (car s)) true)
        (else (element-of-set? x (cdr s)))))

; just cons regardless of whether it exists in the set already
; O(1)
(define adjoin-set cons)

; has to do the same thing as the regular set case
; but this happens to not be commutative
; (the number of duplicates depends only on what's in the first set)
; O(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; just stick them all together
; O(n) where n is the length of the first argument
(define union-set append)
