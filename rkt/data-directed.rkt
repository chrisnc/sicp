; Section 2.4.3, Data-Directed Programming and Additivity, page 242

(define (install-rectangular-package)
  ; internal procedures
  (define (real z) (car z))
  (define (imag z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mag z)
    (sqrt (+ (square (real z))
             (square (imag z)))))
  (define (ang z)
    (atan (imag z) (real z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real '(rectangular) real)
  (put 'imag '(rectangular) imag)
  (put 'mag '(rectangular) mag)
  (put 'ang '(rectangular) ang)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  ; internal procedures
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real z) (* (mag z) (cos (ang z))))
  (define (imag z) (* (mag z) (sin (ang z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real '(polar) real)
  (put 'imag '(polar) imag)
  (put 'mag '(polar) mag)
  (put 'ang '(polar) ang)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: apply-generic"
          (list op type-tags))))))

(define (real z) (apply-generic 'real z))
(define (imag z) (apply-generic 'imag z))
(define (mag z) (apply-generic 'mag z))
(define (ang z) (apply-generic 'ang z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
