; Common procedures for data-directed programming with type coercion

(module data-directed racket/base
  (provide apply-generic-coerce put-coercion get-coercion)

  (require "dd-common.rkt")

  (define coercion-table (make-hash))

  (define (get-coercion source-type target-type)
    (hash-ref coercion-table (cons source-type target-type) #f))

  (define (put-coercion source-type target-type fn)
    (hash-set! coercion-table (cons source-type target-type) fn))

  ; original implementation from page 265
  ; (define (apply-generic-coerce op . args)
  ;   (let ((type-tags (map type-tag args)))
  ;     (let ((proc (get op type-tags)))
  ;       (if proc
  ;         (apply proc (map contents args))
  ;         (if (= (length args) 2)
  ;           (let ((type1 (car type-tags))
  ;                 (type2 (cadr type-tags))
  ;                 (a1 (car args))
  ;                 (a2 (cadr args)))
  ;             (let ((t1->t2 (get-coercion type1 type2))
  ;                   (t2->t1 (get-coercion type2 type1)))
  ;               (cond (t1->t2
  ;                       (apply-generic-coerce op (t1->t2 a1) a2))
  ;                     (t2->t1
  ;                       (apply-generic-coerce op a1 (t2->t1 a2)))
  ;                     (else (error "No method for these types"
  ;                                  (list op type-tags))))))
  ;           (error "No method for these types"
  ;                  (list op type-tags)))))))

  ; Exercise 2.81, part c, page 271
  ;(define (apply-generic-coerce op . args)
  ;  (let ((type-tags (map type-tag args)))
  ;    (let ((proc (get op type-tags)))
  ;      (if proc
  ;        (apply proc (map contents args))
  ;        (if (= (length args) 2)
  ;          (let ((type1 (car type-tags))
  ;                (type2 (cadr type-tags)))
  ;            (if (eq? type1 type2) ; add this check and error early
  ;              (error "No method for these types"
  ;                     (list op type-tags))
  ;              (let ((t1->t2 (get-coercion type1 type2))
  ;                    (t2->t1 (get-coercion type2 type1))
  ;                    (a1 (car args))
  ;                    (a2 (cadr args)))
  ;                (cond (t1->t2
  ;                        (apply-generic-coerce op (t1->t2 a1) a2))
  ;                      (t2->t1
  ;                        (apply-generic-coerce op a1 (t2->t1 a2)))
  ;                      (else (error "No method for these types"
  ;                                   (list op type-tags)))))))
  ;          (error "No method for these types"
  ;                 (list op type-tags)))))))

  ; Exercise 2.82, page 271
  ; This function will determine all possible conversions of the
  ; input types to other types present in the input, and attempt
  ; to find a procedure that exists for some conversion, and applies it.
  ; If we only attempt to coerce all the arguments to the type of any
  ; given argument, we won't match any procedure that exists for arguments
  ; of different types. For example, say we have a procedure for types
  ; '(a b c), and conversions exist for any two of a, b, and c. If we
  ; attempt to apply this procedure to arguments of type '(b c a)
  ; we would not attempt to convert each argument to the appropriate type
  ; given what the procedure will accept, because we have assumed that we
  ; need all types to match to find any suitable procedures.
  ; Note that the solution below will not find procedures that require
  ; converting an argument to a type that is not present among the other arguments.
  ; E.g., if a procedure exists for '(a c) and we are given '(a b) and a conversion
  ; exists from b to c, we will not attempt this conversion.
  (define (apply-generic-coerce op . args)
    (define (get-targets t all-types)
      (filter (lambda (nt) (or (eq? t nt) (get-coercion t nt))) all-types))
    (define (get-possible-types type-tags all-types)
      (if (null? type-tags) '(())
        (let ((t (car type-tags)))
            (foldr append '()
                   (map
                     (lambda (r) (map (lambda (x) (cons x r))
                                      (get-targets t all-types)))
                     (get-possible-types (cdr type-tags) all-types))))))
    (define (get-target-types op possible-types)
      (if (null? possible-types)
        #f
        (let ((proc (get op (car possible-types))))
          (if proc
            (car possible-types)
            (get-target-types op (cdr possible-types))))))
    (define (get-coercions types target-types)
      (map (lambda (t nt)
             (if (eq? t nt)
               (lambda (x) x)
               (get-coercion t nt)))
           types target-types))
    (let ((type-tags (map type-tag args)))
      (let ((target-types (get-target-types op (get-possible-types type-tags type-tags))))
        (if target-types
          (apply (get op target-types)
                 (map contents (map apply (get-coercions type-tags target-types) (map list args))))
          (error "No method for these types"
                 (list op type-tags))))))
  )
