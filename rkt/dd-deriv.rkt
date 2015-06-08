; Exercise 2.73, page 248

(require "dd-common.rkt")

(define (deriv e v)
  (cond ((number? e) 0)
        ((variable? e)
         (if (same-variable? e v) 1 0))
        ((sum? e)
         (make-sum (deriv (addend e) v)
                   (deriv (augend e) v)))
        ((product? e)
         (make-sum (make-product
                     (multiplier e)
                     (deriv (multiplicand e) v))
                   (make-product
                     (deriv (multiplier e) v)
                     (multiplicand e))))
        (else (error "unknown expression type:
                      deriv" e))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? expr num) (and (number? expr) (= expr num)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define addend cadr)
(define augend caddr)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define multiplier cadr)
(define multiplicand caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)

(define (make-exponentiation u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        ((and (number? u) (number? n)) (expt u n))
        (else (list '** u n))))


; part a, page 249

(define (deriv e v)
  (cond ((number? e) 0)
        ((variable? e) (if (same-variable? e v) 1 0))
        (else ((get 'deriv (operator e))
               (operands e) v))))

(define (operator e) (car e))
(define (operands e) (cdr e))

; This transformation essentially delegates the deriv procedure of the sum and
; product types to the implementations that are installed in the data-directed
; fashion. The expression (get 'deriv (operator e)) returns an implementation of
; the deriv function, which is applied to the arguments (operands e) and v.
; We subsume the implementations of deriv for numbers and variables into the
; data-directed model because these data are in an untagged form, unlike sums
; and products, which are implicitly tagged by the operand.

; part b, page 249

(define (install-sum-deriv)
  ; internal procedures
  (define (sum-deriv terms v)
    (make-sum (deriv (car terms) v)
              (deriv (cadr terms) v)))
  ; interface to the rest of the system
  (put 'deriv '+ sum-deriv)
  'done)

(define (install-prod-deriv)
  ; internal procedures
  (define (prod-deriv terms v)
    (make-sum (make-product
                (car terms)
                (deriv (cadr terms) v))
              (make-product
                (deriv (car terms) v)
                (cadr terms))))
  ; interface to the rest of the system
  (put 'deriv '* prod-deriv)
  'done)

; part c, page 250

(define (install-exponent-deriv)
  ; internal procedures
  (define (exp-deriv terms v)
    (make-product
      (cadr terms)
      (make-product
        (make-exponentiation
          (car terms)
          (- (cadr terms) 1))
        (deriv (car terms) v))))
  ; interface to the rest of the system
  (put 'deriv '** exp-deriv)
  'done)

; part d, page 250

; If we index the procedures by operand and then by procedure, we would need to
; change all invocations of the 'put' function to use this same convention, but
; otherwise the code is unchanged. All we are doing is transposing the
; type-method table.
