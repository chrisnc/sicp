; Section 1.3.2, Constructing Procedures Using Lambda, page 83

(include "sumprod.scm")

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       (lambda (x) (+ x 4))
       a
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (f-with-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; Exercise 1.34, page 88
(define (f g) (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))
; (f f)
; -> (f 2)
; -> (2 2)
; -> fails because 2 is not a procedure

; Section 1.3.3, Procedures as General Methods, page 89

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (average x y) (/ (+ x y) 2.0))

(define (search f negp posp)
  (let ((midp (average negp posp)))
    (if (close-enough? negp posp)
      midp
      (let ((test-value (f midp)))
        (cond ((positive? test-value)
               (search f negp midp))
              ((negative? test-value)
               (search f midp posp))
              (else midp))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (sqrt-fixed x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; Exercise 1.35, page 94
; x -> 1 + 1/x
; x = 1 + 1/x
; x^2 = x + 1
; x^2 - x - 1 = 0
; x = (1 + sqrt(1 + 4)) / 2
; x = (1 + sqrt(5)) / 2 = phi

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; Exercise 1.36, page 94
(define (fixed-point-trace f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define xx-sol
  (fixed-point-trace (lambda (x) (/ (log 1000) (log x))) 2.0))
(define xx-sol-avg
  (fixed-point-trace (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0))
