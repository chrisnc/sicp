; Example 2.3.4: Huffman Encoding Trees, page 218

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; put the symbol list and weight first to make it easier to read
; (left and right will contain more structure)
(define (make-code-tree left right)
  (list (append (symbols-tree left) (symbols-tree right))
        (+ (weight-tree left) (weight-tree right))
        left
        right))

(define (left-branch tree) (caddr tree))
(define (right-branch tree) (cadddr tree))
(define (symbols-tree tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (car tree)))
(define (weight-tree tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: choose-branch" bit))))

(define (adjoin-set x s)
  (cond ((null? s) (list x))
        ((< (weight-tree x) (weight-tree (car s))) (cons x s))
        (else (cons (car s)
                    (adjoin-set x (cdr s))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)   ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))


; Exercise 2.67, page 226
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


; Exercise 2.68, page 226
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (contains-symbol? syms s)
  (cond ((null? syms) #f)
        ((eq? s (car syms)) #t)
        (else (contains-symbol? (cdr syms) s))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         '())
        ((contains-symbol? (symbols-tree (left-branch tree)) symbol)
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains-symbol? (symbols-tree (right-branch tree)) symbol)
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "this symbol can't be encoded" symbol))))


; Exercise 2.69, page 227
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge s)
  (cond
    ((null? s) (error "no trees to merge"))
    ((null? (cdr s)) (car s))
    (else
     (successive-merge
       (adjoin-set
         (make-code-tree
           (cadr s) ; cadr first to bias frequent symbols to 0
           (car s))
         (cddr s))))))

(define (single-merge s)
  (if (or (null? s) (null? (cdr s)))
    s
    (make-code-tree
      (make-code-tree (car s) (cadr s))
      (cddr s))))

; Exercise 2.70, page 228
(define fifties-rock-tree
  (generate-huffman-tree
    '((A    2)
      (GET  2)
      (SHA  3)
      (WAH  1)
      (BOOM 1)
      (JOB  2)
      (NA  16)
      (YIP  9))))

(define fifties-rock-message
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(length (encode fifties-rock-message fifties-rock-tree))
; This message can be encoded in 84 bits using the fifties-rock-tree.
; If we used the fixed-length encoding, with 3 bits per symbol,
; we could encode the message with 36 * 3 = 108 bits.


; Exercise 2.71
; With alphabets where the weights are 1,2,4,...,2^{n-1}, the generated Huffman
; tree will always have a long left (or right) spine, because we always merge
; only one additional symbol (the one with the next largest weight).
; This will lead to only one bit being required to encode the most frequent
; symbol, and n - 1 bits needed to encode the least frequent.

; Exercise 2.72

; In the special case of frequencies given by 1,2,4,...,2^{n-1}, the
; number of steps needed to encode the most frequent symbol will be O(1).
; The root of the tree will have the left branch with just the most frequent
; symbol (since its weight outweights the others combined), and we emit a 0
; before even looking at the right branch.
; For the least frequent symbol, we will make n - 1 choices of which branch
; to take, and at each node the total number of elements in lists we inspect
; decreases by 1, starting with n. (We will look at the most frequent element's
; branch first, then the list of the remaining elements.)
; we will traverse lists with length decreasing by 1
; until we reach the leaf for the least frequent symbol. This has a runtime of
; (n - 1) * sum_{i=2}^n i \in O(n^2).
; If we implement the procedures such that in addition to always inspecting the
; left branch first, we assume that elements not represented in the left branch
; are contained in the right branch, (i.e., we assume that the caller has not
; given a symbol not present in the tree), then encoding the least frequent
; symbol can be done in O(n).
