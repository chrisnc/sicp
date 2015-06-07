; Exercise 2.74, page 250

(require racket/include)
(include "dd-common.rkt")

(define (install-computer-division)
  (define (get-record employee-name file)
    (hash-ref file employee-name #f))
  (define get-salary car)
  (define (set-record file employee-name salary address)
    (hash-set! file employee-name (cons salary address)))
  (define create-file make-hash)
  (put 'get-salary 'computer-division get-salary)
  (put 'get-record 'computer-division get-record)
  (put 'set-record 'computer-division set-record)
  (put 'create-file 'computer-division create-file)
  'done)

; part a
(define (get-record employee-name generic-file)
  ((get 'get-record (type-tag generic-file))
   employee-name (contents generic-file)))

; Each personnel file should be structured as a pair where the car is a
; type-tag identifying the division, and the cdr is an arbitrary structure
; implementing the division's file format.


; part b
(define (get-salary employee-name generic-file)
  (let ((tag (type-tag generic-file)))
    (let ((record ((get 'get-record tag) employee-name (contents generic-file))))
      ((get 'get-salary tag) record))))

; part c
(define (find-employee-record employee-name files)
  (if (null? files) '()
    (let ((record (get-record employee-name (car files))))
      (if (null? record) (find-employee-record employee-name (cdr files))
        record))))

; put it all together
(define (set-record generic-file employee-name salary address)
  (let ((tag (type-tag generic-file)))
    ((get 'set-record tag) (contents generic-file) employee-name salary address)))

(define (create-file tag)
  (cons tag ((get 'create-file tag))))

(install-computer-division)
(define cfile (create-file 'computer-division))
(set-record cfile "Bill" 123 "foo")
(display (get-record "Bill" cfile))
(newline)
(display (get-salary "Bill" cfile))
(newline)
(display (find-employee-record "Bill" (list cfile)))
(newline)

; part d
; The changes that must be made are to give the new division(s) a unique type
; tag to use, and then to install the get-record and get-salary procedures in
; the generic framework using that type tag. Then the type tag must be consed to
; the division's files when they are provided to the generic system.
