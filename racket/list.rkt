; Section 2.2.1 Representing Sequences, page 134

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (length items)
  (define (iter a count)
    (if (null? a)
      count
      (iter (cdr a) (+ 1 count))))
  (iter items 0))

(define (append a b)
  (if (null? a)
    b
    (cons (car a) (append (cdr a) b))))

; Exercise 2.17, page 139
(define (last-pair l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))))

; Exercise 2.18, page 140
(define (reverse l)
  (define (iter r l)
    (cond ((null? l) r)
          (else (iter (cons (car l) r) (cdr l)))))
  (iter '() l))

; Exercise 2.19, page 140
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

; The order of the coins does not matter, since the two paths of the recursion
; capture all possible ways to use the coins to produce amount.
; Either a combination does not use any of a particular coin, or it uses
; one or more of that coin, and some number of the other coins, in either case.
; No possibility is excluded by encountering a particular denomination first.

; Exercise 2.20, page 141
(define (same-parity x . l)
  (define (parity-rest l)
    (cond ((null? l) '())
          ((= (remainder x 2) (remainder (car l) 2))
           (cons (car l) (parity-rest (cdr l))))
          (else (parity-rest (cdr l)))))
  (cons x (parity-rest l)))

; Mapping over lists, page 143
(define (scale-list items factor)
  (if (null? items)
    '()
    (cons (* (car items) factor)
          (scale-list (cdr items)
                      factor))))

(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

; Exercise 2.21, page 144
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

; Exercise 2.22, page 145

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items '()))

; the list answer is constructed by consing the car of things with answer
; for each element in items, so the items are appended to the head of answer
; in the order they are encountered in items

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items '()))

; we can fix it by just calling reverse on the result of the first
; implementation
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (reverse (iter items '())))

; this doesn't work either because we are consing a list with a single item
; rather than the other way around, so the result will not be a list

; Exercise 2.23, page 146
(define (for-each f l)
  (cond ((null? l) #t)
        (else (f (car l)) (for-each f (cdr l)))))
