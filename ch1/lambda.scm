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

;(define xx-sol
;  (fixed-point-trace (lambda (x) (/ (log 1000) (log x))) 2.0))
;(define xx-sol-avg
;  (fixed-point-trace (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0))

; Exercise 1.37 (a), page 94
(define (cont-frac n d k)
  (define (cfrack i result)
    (if (= i 0)
      result
      (cfrack (- i 1) (/ (n i) (+ (d i) result)))))
  (cfrack k 0))

(define phi-cfrac (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))

; Exercise 1.37 (b), page 95
; recursive process version
(define (cont-frac-rec n d k)
  (define (cfrack i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (cfrack (+ i 1))))))
  (cfrack 1))

; Exercise 1.38, page 96
(define (euler-frac k)
  (cont-frac
    (lambda (i) 1.0)
    (lambda (i)
      (cond ((= 0 (remainder (- i 2) 3)) (/ (* 2 (+ i 1)) 3))
            (else 1)))
    k))

; Exercise 1.39, page 96
(define (tan-cf x k)
  (cont-frac
    (lambda (i)
      (if (= i 1)
        x
        (- (* x x))))
    (lambda (i) (- (* i 2) 1))
    k))

; Section 1.3.4, Procedures as Returned Values, page 97
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-fixed-avg x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method
    (lambda (y) (- (square y) x))
    1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fpot-avg-damp x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-fpot-newton x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))

; Exercise 1.40, page 103
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

; Exercise 1.41, page 103
(define (double f)
  (lambda (x) (f (f x))))

; Exercise 1.42, page 103
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43, page 104
(define (repeated f n)
  (define (iter fr n)
    (if (= n 1)
      fr
      (iter (compose f fr) (- n 1))))
  (iter f n))

; Exercise 1.44, page 104
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (smooth-n f n)
  ((repeated smooth n) f))

; Exercise 1.45, page 105
(define (nth-root n x)
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1))))
    (repeated average-damp n)
    x))

; Exercise 1.46, page 105
(define (iterative-improve good-enough improve)
  (define (iter x)
    (if (good-enough x)
      x
      (iter (improve x))))
  iter)

(define (sqrt-imp x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess))))
  1.0))

(define (fixed-point-imp f first-guess)
  ((iterative-improve
     (lambda (guess) (< (abs (- guess (f guess))) tolerance))
     f)
   first-guess))

(define (newtons-method-imp g guess)
  (fixed-point-imp (newton-transform g) guess))
