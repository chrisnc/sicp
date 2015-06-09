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
  (define (apply-generic-coerce op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags)))
              (if (eq? type1 type2) ; add this check and error early
                (error "No method for these types"
                       (list op type-tags))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (cond (t1->t2
                          (apply-generic-coerce op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic-coerce op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags)))))))
            (error "No method for these types"
                   (list op type-tags)))))))


  )
