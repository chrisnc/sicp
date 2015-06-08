; Section 2.5.1, Generic Arithmetic Operations

(require "dd-common.rkt")

(require "dd-complex.rkt")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number tag)
  (put 'equ? '(scheme-number scheme-number) =)
  )

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ; internal procedures
  (define numer car)
  (define denom cdr)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equal? (numer x) (numer y))
                          (equal? (denom x) (denom y)))))
  )

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

(define (install-complex-package)
  ; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real z1) (real z2))
                         (+ (imag z1) (imag z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real z1) (real z2))
                         (- (imag z1) (imag z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (mag z1) (mag z2))
                       (+ (ang z1) (ang z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (mag z1) (mag z2))
                       (- (ang z1) (ang z2))))
  ; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; Exercise 2.77, page 260
  ; This works because real, imag, mag, and ang were defined in the dd-complex
  ; package to dispatch using apply-generic. The following occurs when we call
  ; 1. (mag '(complex rectangular 3 . 4))
  ; 2. (apply-generic 'mag '(complex rectangular 3 . 4))
  ; 3. (mag '(rectangular 3 . 4))
  ; 4. (apply-generic 'mag '(rectangular 3 . 4))
  ; 5. (mag '(3 . 4))
  ; 6. 5

  ; apply-generic is invoked twice. The first time it strips off the complex
  ; tag and dispatches to the same implementation of mag.  The second time it
  ; strips off the rectangular tag and dispatches to the implementation of mag
  ; that is internal to the rectangular package, which actually computes the
  ; magnitude.

  (put 'real '(complex) real)
  (put 'imag '(complex) imag)
  (put 'mag '(complex) mag)
  (put 'ang '(complex) ang)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equal? (real x) (real y))
                          (equal? (imag x) (imag y)))))
  )
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Exercise 2.79
; equ? implementations installed in the above packages
(define (equ? x y)
  (apply-generic 'equ? x y))
