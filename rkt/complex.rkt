; Section 2.4.1, Representations for Complex Numbers, page 232

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


; Implementing tagged data
(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: type-tag" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: contents" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))


; Ben Bitdiddle's representation
(define (square x) (* x x))

(define real-rectangular car)
(define imag-rectangular cdr)
(define (mag-rectangular z)
  (sqrt (+ (square (real-rectangular z))
           (square (imag-rectangular z)))))
(define (ang-rectangular z)
  (atan (imag-rectangular z) (real-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; Alyssa P. Hacker's representation
(define (real-polar z) (* (mag-polar z) (cos (ang-polar z))))
(define (imag-polar z) (* (mag-polar z) (sin (ang-polar z))))
(define mag-polar car)
(define ang-polar cdr)
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


(define (real z)
  (cond ((rectangular? z)
         (real-rectangular (contents z)))
        ((polar? z)
         (real-polar (contents z)))
        (else (error "Unknown type: real" z))))

(define (imag z)
  (cond ((rectangular? z)
         (imag-rectangular (contents z)))
        ((polar? z)
         (imag-polar (contents z)))
        (else (error "Unknown type: imag" z))))

(define (mag z)
  (cond ((rectangular? z)
         (mag-rectangular (contents z)))
        ((polar? z)
         (mag-polar (contents z)))
        (else (error "Unknown type: mag" z))))

(define (ang z)
  (cond ((rectangular? z)
         (ang-rectangular (contents z)))
        ((polar? z)
         (ang-polar (contents z)))
        (else (error "Unknown type: angle" z))))

(define make-from-real-imag make-from-real-imag-rectangular)

(define make-from-mag-ang make-from-mag-ang-polar)
