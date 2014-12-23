(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
           (exponent expr)
           (make-product
             (make-exponentiation
               (base expr)
               (- (exponent expr) 1))
             (deriv (base expr) var))))
        (else
          (error "unknown expression type: DERIV" expr))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num) (and (number? expr) (= expr num)))

(define (left-expr expr op)
  (let ((sub (takef expr (lambda (s) (not (eq? s op))))))
    (cond ((= (length sub) 1) (car sub))
          (else sub))))

(define (right-expr expr op)
  (let ((sub (cdr (dropf expr (lambda (s) (not (eq? s op)))))))
    (cond ((= (length sub) 1) (car sub))
          (else sub))))

; Exercise 2.58, page 204
; part b
; if the expression contains any '+' in it, it's a sum
(define (sum? x) (and (pair? x) (memq '+ x)))
(define (addend expr) (left-expr expr '+))
(define (augend expr) (right-expr expr '+))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (or (sum? a1) (product? a1) (exponentiation? a1))
              (or (sum? a2) (product? a2) (exponentiation? a2)))
         (append a1 (cons '+ a2)))
        ; do some constant folding
        ((and (number? a1) (sum? a2) (number? (addend a2)))
         (make-sum (+ a1 (addend a2)) (augend a2)))
        ((and (number? a1) (or (sum? a2) (product? a2) (exponentiation? a2)))
         (cons a1 (cons '+ a2)))
        ((and (number? a2) (or (sum? a1) (product? a1) (exponentiation? a1)))
         (append a1 (list '+ a2)))
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

; an expression is a product if it contains no additions
(define (product? x) (and (pair? x) (eq? (cadr x) '*) (not (memq '+ x))))
(define (multiplier expr) (left-expr expr '*))
(define (multiplicand expr) (right-expr expr '*))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (or (product? m1) (exponentiation? m1))
              (or (product? m2) (exponentiation? m2)))
         (append m1 (cons '* m2)))
        ; do some constant folding
        ((and (number? m1) (product? m2) (number? (multiplier m2)))
         (make-product (* m1 (multiplier m2)) (multiplicand m2)))
        ((and (number? m1) (or (product? m2) (exponentiation? m2)))
         (cons m1 (cons '* m2)))
        ((and (number? m2) (or (product? m1) (exponentiation? m1)))
         (cons m2 (cons '* m1)))
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; an expression is an exponentiation if it contains no additions or
; multiplications
(define (exponentiation? x) (and (pair? x) (eq? (cadr x) '**) (not (memq '+ x))
                                 (not (memq '* x))))
; these don't need to change, because we don't deal with iterated exponentiation
(define base car)
(define exponent caddr)

(define (make-exponentiation u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        ((and (number? u) (number? n)) (expt u n))
        (else (list u '** n))))
