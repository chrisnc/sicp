; Section 2.2.2, Hierarchical Structures, page 147

(define (count-leaves l)
  (cond ((null? l) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Exercise 2.24, page 149
(list 1 (list 2 (list 3 4)))

; result from interpreter
; '(1 (2 (3 4)))

; as a tree
;       .
;      / \
;     1   .
;        / \
;       2   .
;          / \
;         3   4

; Exercise 2.25, page 149
(display (cadr (caddr '(1 3 (5 7) 9))))
(newline)
(display (caar '((7))))
(newline)
(display (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))
(newline)

; Exercise 2.26, page 150
(define x (list 1 2 3))
(define y (list 4 5 6))

(display (append x y)) ; (1 2 3 4 5 6)
(newline)
(display (cons x y)) ; ((1 2 3) 4 5 6)
(newline)
(display (list x y)) ; ((1 2 3) (4 5 6))
(newline)

; Exercise 2.27, page 150
(define (deep-reverse l)
  (define (iter r l)
    (cond ((null? l) r)
          ((pair? (car l)) (iter (cons (deep-reverse (car l)) r) (cdr l)))
          (else (iter (cons (car l) r) (cdr l)))))
  (iter '() l))

; Exercise 2.28, page 150
(define (fringe l)
  (cond ((null? l) '())
        ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

; Exercise 2.29, page 151
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

; part a
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

; part b
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; part c
(define (balanced-branch branch)
  (if (pair? (branch-structure branch))
    (balanced-mobile (branch-structure branch))
    #t))

(define (balanced-mobile mobile)
  (and (= (* (branch-length (left-branch mobile))
             (branch-weight (left-branch mobile)))
          (* (branch-length (right-branch mobile))
             (branch-weight (right-branch mobile))))
       (balanced-branch (left-branch mobile))
       (balanced-branch (right-branch mobile))))

; part d
; If we redefine the constructors, we would just need to redefine the selectors
; as well. The other functions are entirely in terms of these selectors, and the
; pair? predicate will still distinguish between a structure that is just a
; weight and one that is a mobile, since mobiles are still pairs
;(define left-branch car)
;(define right-branch cdr)
;(define branch-length car)
;(define branch-structure cdr)
; Only right-branch and branch-structure are different.

; Mapping over trees, page 152
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

; Exercise 2.30, page 153
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

; Exercise 2.31, page 153
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

; Exercise 2.32, page 154
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; This works by letting rest be the set of all subsets of s that do not contain
; the first element of s, and appending this to the list containing each element
; of rest but with the first element tacked on. In other words, for every subset
; in rest, we have the original subset without the first element and another
; subset with the first element. The concatenation of these two lists of subsets
; will contain all possible subsets.
