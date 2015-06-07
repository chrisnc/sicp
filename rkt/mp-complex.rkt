; Exercise 2.75, complex number constructor in message passing style page 253

(define (square x) (* x x))

; Example from page 252
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; and now the exercise
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Uknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)


; Exercise 2.76, page 253
;
; Generic operations with explicit dispatch:
;   - To add a new type, each generic operation must be modified to check for
;     the new type and call the appropriate function for that type's version of
;     the operation.
;   - To add a new operation, a new generic operation must be made, and each
;     type in the system must gain an implementation for that operation, that
;     the generic operation will dispatch to.
;
; Data-directed style:
;   - To add a new type, we write an installation procedure that provides
;     implementations of each existing operation.
;   - To add a new operation, we write an installation procedure that provides
;     implementations for the new operation for each existing type.
;
; Message-passing style:
;   - To add a new type, the type only needs to respond to the possible operations.
;   - To add a new operation, each type in the system must be modified to respond to the new operation.
;
; When new types must often be added, message-passing style is most convenient,
; because no changes to existing types or operations are required.
;
; When new operations must often be added, an explicit dispatch on type may
; make the most sense, because the new implementations will be confined to the
; one place where the dispatch is performed for that operation.
; As in rkt/complex.rkt, each additional operation consisted of implementing a
; single function, which checked the type, and then produced the result.
;
; Data-directed style can support both cases, as it allows us to deal
; explicitly with adding new rows and columns in the dispatch table. Adding a
; new operation can consist of writing an installation procedure that grants
; implementations of the new operation to each existing type, and likewise
; adding a new type consists of writing an installation procedures that grants
; implementations of all existing operations to the new type.
