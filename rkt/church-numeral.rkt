; Exercise 2.6, page 126

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; implementing one
; trivial definition
;(define one (add-1 zero))
; substitute definition of add-1
;(define one (lambda (f) (lambda (x) (f ((zero f) x)))))
; substitute definition of zero
;(define one (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
; apply a lambda to its arg
;(define one (lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
; and again
(define one (lambda (f) (lambda (x) (f x))))

; implementing two
; trivial definition
;(define two (add-1 one))
; substitute definition of add-1
;(define two (lambda (f) (lambda (x) (f ((one f) x)))))
; substitute definition of one
;(define two (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))))
; apply lambda to its arg
;(define two (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x)))))
; and again
(define two (lambda (f) (lambda (x) (f (f x)))))

; implementing add
; each number is applied to the procedure, each giving a procedure, which are
; then applied to the argument x in succession
(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))
