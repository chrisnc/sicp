; Exercise 2.81, page 270 procedures for type coercion.

(require "dd-common.rkt")
(require "dd-coerce.rkt")
(require "dd-numbers.rkt")

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

; part a, page 271

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic-coerce 'exp x y))
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (expt x y)))

; The given implementation of apply-generic-coerce will enter an infinite loop
; when given two arguments of the same type but an operation that doesn't exist
; on that type. If the operation does exist on the type, it never tries to coerce.
; Therefore (exp 3 4) works fine, but
; (exp (complex rectangular 3 . 4) (complex rectangular 3 . 4)) loops.

; part b, page 271
; Louis is not correct that something had to be done about coercion with
; arguments of the same type. The existing procedure will look for a coercion
; procedure only after it can't find an operation for the given types.
; If it also can't find a coercion procedure from a type to itself, it will just exit with an error.
; It is only by adding a coercion procedure from a type to itself is a loop introduced.

; part c, implemented in rkt/dd-coerce.rkt
