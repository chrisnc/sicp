; Common procedures for data-directed programming.

(module dd-common racket/base
  (provide get put type-tag contents attach-tag apply-generic)

  (define dd-table (make-hash))

  (define (get op types)
    (hash-ref dd-table (cons op types) #f))

  (define (put op types fn)
    (hash-set! dd-table (cons op types) fn))

  ; Exercise 2.78, page 261
  ; modified to distinguish between integers and real numbers
  ; by convention, don't attach tags to integers and reals
  (define (type-tag x)
    (cond ((exact-integer? x) 'integer)
          ((real? x) 'real)
          (else (car x))))
  (define (contents x)
    (cond ((number? x) x)
          (else (cdr x))))
  (define attach-tag cons)

  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: apply-generic"
            (list op type-tags))))))
  )
