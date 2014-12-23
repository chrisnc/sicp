; Section 2.2.3, Sequences as Conventional Interfaces, page 154

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not? (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      '()
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 0
    (map square
         (filter odd?
                 (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons '()
    (filter even?
            (map fib
                 (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
    cons '()
    (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(define (salary-of-highest-paid-programmer records)
  (accumulate max 0 (map salary (filter programmer? records))))

; Exercise 2.33, page 161
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append-new seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Exercise 2.34, page 162
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; Exercise 2.35, page 163
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

; Exercise 2.36, page 163
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37, page 163
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product v r)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (a) (matrix-*-vector cols a)) m)))

; Exercise 2.38, page 165
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

; (fold-right / 1 (list 1 2 3))
; 3/2
; (fold-left / 1 (list 1 2 3))
; 1/6
; (fold-right list '() (list 1 2 3))
; '(1 (2 (3 ())))
; (fold-left list '() (list 1 2 3))
; '(((() 1) 2) 3)

; fold-right and fold-left produce the same value for any sequence only when op
; is associative a fold is just interspersing elements of the sequence with op
; where the fold decides how the operands will associate with an associative
; operator, it does not matter how the operands are associated, by definition

; Exercise 2.39, page 166
(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
