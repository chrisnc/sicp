; Common procedures for data-directed programming.

(module data-directed racket/base
  (provide get put type-tag contents attach-tag apply-generic)

  (define dd-table (make-hash))

  (define (get op types)
    (hash-ref dd-table (cons op types) #f))

  (define (put op types fn)
    (hash-set! dd-table (cons op types) fn))

  (define type-tag car)
  (define contents cdr)
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
