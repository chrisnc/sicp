; Sets as ordered lists, page 208

(define (element-of-set? x s)
  (cond ((null? s) false)
        ((= x (car s)) true)
        ((< x (car s)) false)
        (else (element-of-set? x (cdr s)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1)
                                        (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

; Exercise 2.61, page 210
(define (adjoin-set x s)
  (cond ((null? s) (list x))
        ((< x (car s)) (cons x s))
        ((= x (car s)) s)
        (else (cons (car s) (adjoin-set x (cdr s))))))

; Exercise 2.62, page 210
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      (else      (cons x2 (union-set set1 (cdr set2)))))))))
