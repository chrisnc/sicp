; Exercise 2.74, page 250

(define type-tag car)
(define contents cdr)

; part a

(define (get-record employee-name generic-file)
  ((get 'get-record (type-tag generic-file))
   employee-name (contents generic-file)))

; Each personnel file should be structured as a pair where the car is a
; type-tag identifying the division, and the cdr is an arbitrary structure
; implementing the division's file format.


; part b
(define (get-salary employee-name generic-file)
  (let ((record ((get 'get-record (type-tag generic-file)) employee-name)))
    ((get 'get-salary (type-tag record)) (contents record))))

; part c
(define (find-employee-record employee-name files)
  (if (null? files) '()
    (let ((record (get-record employee-name (car files))))
      (if (null? record) (find-employee-record employee-name (cdr files))
        record))))

; part d
; The changes that must be made are to give the new division(s) a unique type
; tag to use, and then to install the get-record and get-salary procedures in
; the generic framework using that type tag. Then the type tag must be consed to
; the division's files when they are provided to the generic system.
