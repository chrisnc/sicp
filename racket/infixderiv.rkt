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

; Exercise 2.58, page 204
; part a
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define addend car)
(define (augend expr)
  (cond ((= (length expr) 3) (caddr expr))
        ((> (length expr) 3) (cddr expr))
        (else (error "Taking augend of a non-sum"))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define multiplier car)
(define (multiplicand expr)
  (cond ((= (length expr) 3) (caddr expr))
        ((> (length expr) 3) (cddr expr))
        (else (error "Taking multiplicand of a non-product"))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (exponentiation? x) (and (pair? x) (eq? (cadr x) '**)))
(define base car)
(define exponent caddr)

(define (make-exponentiation u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        ((and (number? u) (number? n)) (expt u n))
        (else (list u '** n))))
