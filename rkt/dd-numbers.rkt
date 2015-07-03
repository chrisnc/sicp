; Section 2.5.1, Generic Arithmetic Operations

(module dd-numbers racket/base
  (provide add sub mul div make-rational numer denom
           make-complex-from-real-imag
           make-complex-from-mag-ang
           equ? =zero? real imag mag ang
           tower-raise tower-order
           project tower-drop)

  (require "dd-common.rkt")
  (require "dd-complex.rkt")

  (define (add x y) (apply-generic-tower 'add x y))
  (define (sub x y) (apply-generic-tower 'sub x y))
  (define (mul x y) (apply-generic-tower 'mul x y))
  (define (div x y) (apply-generic-tower 'div x y))

  (define (install-integer-package)
    (put 'add '(integer integer) +)
    (put 'sub '(integer integer) -)
    (put 'mul '(integer integer) *)
    (put 'div '(integer integer) /)
    (put 'equ? '(integer integer) =)
    (put '=zero? '(integer) zero?)
    (put 'project '(integer) (lambda (x) x)))
  (install-integer-package)

  (define (install-real-package)
    (put 'add '(real real) +)
    (put 'sub '(real real) -)
    (put 'mul '(real real) *)
    (put 'div '(real real) /)
    (put 'equ? '(real real) =)
    (put '=zero? '(real) zero?)
    (put 'project '(real)
         (lambda (x)
           (make-rational (inexact->exact x) 1)))
    )
  (install-real-package)


  ; expose these for the tower implementation
  (define rat-numer car)
  (define rat-denom cdr)
  (define (install-rational-package)
    ; internal procedures
    (define (make-rat n d)
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
    (define (add-rat x y)
      (make-rat (+ (* (rat-numer x) (rat-denom y))
                   (* (rat-numer y) (rat-denom x)))
                (* (rat-denom x) (rat-denom y))))
    (define (sub-rat x y)
      (make-rat (- (* (rat-numer x) (rat-denom y))
                   (* (rat-numer y) (rat-denom x)))
                (* (rat-denom x) (rat-denom y))))
    (define (mul-rat x y)
      (make-rat (* (rat-numer x) (rat-numer y))
                (* (rat-denom x) (rat-denom y))))
    (define (div-rat x y)
      (make-rat (* (rat-numer x) (rat-denom y))
                (* (rat-denom x) (rat-numer y))))
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
    (put 'numer '(rational) rat-numer)
    (put 'denom '(rational) rat-denom)
    (put 'equ? '(rational rational)
         (lambda (x y) (and (= (rat-numer x) (rat-numer y))
                            (= (rat-denom x) (rat-denom y)))))
    (put '=zero? '(rational)
         (lambda (x) (and (= (rat-numer x) 0)
                          (not (= (rat-denom x) 0)))))
    (put 'project '(rational)
         (lambda (x)
           (round (inexact->exact (/ (rat-numer x) (rat-denom x))))))
    )

  (install-rational-package)

  (define (make-rational n d)
    ((get 'make 'rational) n d))
  (define (numer x) (apply-generic-tower 'numer x))
  (define (denom x) (apply-generic-tower 'denom x))

  (define (real z) (apply-generic-tower 'real z))
  (define (imag z) (apply-generic-tower 'imag z))
  (define (mag z) (apply-generic-tower 'mag z))
  (define (ang z) (apply-generic-tower 'ang z))
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
         (lambda (x y) (and (= (real x) (real y))
                            (= (imag x) (imag y)))))
    (put '=zero? '(complex)
         (lambda (x) (and (= (real x) 0)
                          (= (imag x) 0))))
    (put 'project '(complex)
         (lambda (x) (* 1.0 (real x))))
    )
  (install-complex-package)
  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) (* 1.0 x) (* 1.0 y)))
  (define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) (* 1.0 r) (* 1.0 a)))

  ; Exercise 2.79, page 261
  ; equ? implementations installed in the above packages
  (define (equ? x y)
    (apply-generic-tower 'equ? x y))

  ; Exercise 2.80, page 261
  ; =zero? implementations installed in the above packages
  (define (=zero? x)
    (apply-generic-tower '=zero? x))

  ; Exercise 2.83, page 272
  (define (install-tower-package)
    (put 'tower-raise '(integer)
         (lambda (x)
           (make-rational x 1)))
    (put 'tower-raise '(rational)
         (lambda (x)
           (* (/ (rat-numer x) (rat-denom x)) 1.0)))
    (put 'tower-raise '(real)
         (lambda (x)
           (make-complex-from-real-imag x 0)))
    (put 'tower-raise '(complex)
         (lambda (x) (attach-tag 'complex x)))
    (put 'tower-order '(integer)
         (lambda (x) 1))
    (put 'tower-order '(rational)
         (lambda (x) 2))
    (put 'tower-order '(real)
         (lambda (x) 3))
    (put 'tower-order '(complex)
         (lambda (x) 4)))
  (install-tower-package)
  (define (tower-raise x)
    (apply-generic 'tower-raise x))
  (define (tower-order x)
    (apply-generic 'tower-order x))
  (define tower-bottom 1)
  (define tower-top 4)

  (define (get-highest args)
    (define (loop remaining current-highest)
      (cond ((null? remaining) current-highest)
            ((> (tower-order (car remaining))
                (tower-order current-highest))
             (loop (cdr remaining) (car remaining)))
            (else (loop (cdr remaining) current-highest))))
    (if (null? args)
      '()
      (loop (cdr args) (car args))))
  (define (raise-to target x)
    (if (<= (tower-order target) (tower-order x))
      x
      (raise-to target (tower-raise x))))

  ; Exercise 2.84, page 272
  (define (apply-generic-tower op . args)
    ; special case for one argument, raise until
    ; it works or fails
    (if (= (length args) 1)
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (cond (proc (apply proc (map contents args)))
                ((= tower-top (tower-order (car args)))
                 (error "No method for these types: apply-generic-tower"
                        (list op type-tags)))
                (else (apply-generic-tower op (tower-raise (car args)))))))
      (let ((raised-args (map (lambda (x) (raise-to (get-highest args) x)) args)))
        (let ((type-tags (map type-tag raised-args)))
          (let ((proc (get op type-tags)))
            (if proc
              (let ((res (apply proc (map contents raised-args))))
                (if (get 'tower-order (list (type-tag res)))
                  (tower-drop res) ; for Exercise 2.85
                  res))
              (error
                "No method for these types: apply-generic-tower"
                (list op type-tags))))))))

  (define (project x) (apply-generic 'project x))

  ; Exercise 2.85, page 272
  (define (tower-drop x)
    (if (= (tower-order x) tower-bottom)
      x ; we enter into a loop without this base case, because integers always project to themselves
      (let ((xd (project x)))
        (if (equ? xd x) ; equ? will raise xd because it uses apply-generic-tower
          (tower-drop xd)
          x))))
  )
