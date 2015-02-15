; Sets as binary trees, page 210

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x s)
  (cond ((null? s) false)
        ((= x (entry s)) true)
        ((< x (entry s))
         (element-of-set? x (left-branch s)))
        ((> x (entry s))
         (element-of-set? x (right-branch s)))))

(define (adjoin-set x s)
  (cond ((null? s) (make-tree x '() '()))
        ((= x (entry s)) s)
        ((< x (entry s))
         (make-tree (entry s)
                    (adjoin-set x (left-branch s))
                    (right-branch s)))
        ((> x (entry s))
         (make-tree (entry s)
                    (left-branch s)
                    (adjoin-set x (right-branch s))))))

; Exercise 2.63, page 213

(define (tree-to-list-1 tree)
  (if (null? tree)
    '()
    (append (tree-to-list-1 (left-branch tree))
            (cons (entry tree)
                  (tree-to-list-1
                    (right-branch tree))))))

(define (tree-to-list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))

; part a
; They are both in-order traversals of the tree, so they will both produce the
; same result list for the same input tree.
;
; part b
; tree-to-list-1 uses append on the result of the left recursive call, so it
; will use a quadratic number of steps in the size of the left subtree of the
; root to convert the result to a list, because it does O(n) work in each
; recursive call, and there are O(n) such calls.
; tree-to-list-2 does only O(1) work in each recursive call, and there are O(n)
; such calls, so tree-to-list-2 is O(n)

; Exercise 2.64, page 215
(define (list-to-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

; part a
; It first decides how many of the n elements to put into a left subtree, then
; calls itself to produce this tree and the list of remaining elements. It
; decides how many elements to put in the right subtree, as n minus the size of
; the left subtree, minus 1 (for the root). It takes the first of the
; non-left-elts for the root of the resulting partial tree, then calls itself
; to produce the right subtree with the appropriate number of elements. It
; returns both the tree formed from the root, left subtree, and right subtree,
; as well as the remaining elements not consumed by the second recursive call.

; part b
; The work done in partial-tree excluding the recursive calls is O(1), and there
; will be O(n) total calls to partial-tree altogether. Every call made with n >
; 0 will consume a different element in the tree for its root, so there are n
; such calls, and each call with n > 0 can make at most two calls with n = 0.
; Therefore list-to-tree is O(n).

; Exercise 2.65, page 216
(define (union-set set1 set2)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((< (car a) (car b)) (cons (car a) (merge (cdr a) b)))
          ((= (car a) (car b)) (cons (car a) (merge (cdr a) (cdr b))))
          (else                (cons (car b) (merge a       (cdr b))))))
  (list-to-tree (merge (tree-to-list-2 set1) (tree-to-list-2 set2))))

(define (intersection-set set1 set2)
  (define (merge a b)
    (cond ((or (null? a) (null? b)) '())
          ((= (car a) (car b))
           (cons (car a) (merge (cdr a) (cdr b))))
          ((< (car a) (car b))
           (merge (cdr a) b))
          (else
           (merge a (cdr b)))))
  (list-to-tree (merge (tree-to-list-2 set1) (tree-to-list-2 set2))))
