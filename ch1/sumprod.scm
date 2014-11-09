; Section 1.3, Formulating Abstractions with Higher-Order Procedures, page 74

(define (sum term next a b)
  (if (> a b)
    0 (+ (term a)
       (sum term next (next a) b))))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube inc a b))

(define (id x) x)
(define (inc x) (+ x 1))

(define (sum-integers a b)
  (sum id inc a b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term pi-next a b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b)
     dx))

; Exercise 1.29: Simpson's Rule, page 80
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (c k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (simpson-term k) (* (c k) (y k)))
  (* (/ h 3) (sum-new simpson-term inc 0 n)))

; Exercise 1.30, page 80
(define (sum-new term next a b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31 (a), page 80
(define (product term next a b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n) (product id inc 1 n))

(define (pi-prod n)
  (define (pi-next x)
    (+ x 2))
  (define (pi-term x)
    (* x x))
  (* 8 (/ (product pi-term pi-next 4 n) n (product pi-term pi-next 3 (- n 1)))))

; Exercise 1.31 (b), page 81
; recursive process version
(define (productrec term next a b)
  (if (> a b)
    1
    (* (term a) (productrec term next (next a) b))))

; Exercise 1.32 (a), page 81
(define (accumulate combine nv term next a b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combine result (term a)))))
  (iter a nv))

(define (sumacc term next a b)
  (accumulate + 0 term next a b))

(define (productacc term next a b)
  (accumulate * 1 term next a b))

; Exercise 1.32 (b), page 82
; recursive process version
(define (accumulaterec combine nv term next a b)
  (if (> a b)
    nv
    (combine (term a) (accumulaterec combine nv term next (next a) b))))

; Exercise 1.33, page 82
(define (filtered-accumulate combine nv term next filt a b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filt a) (iter (next a) (combine result (term a))))
          (else (iter (next a) result))))
  (iter a nv))

; Exercise 1.33 (a), page 82
(include "prime.scm")

(define (sum-sq-primes a b)
  (filtered-accumulate + 0 square inc prime? a b))

; Exercise 1.33 (b), page 82
(define (prod-rel-prime n)
  (filtered-accumulate * 1 id inc (lambda (x) (= (gcd x n) 1)) 1 (- n 1)))
