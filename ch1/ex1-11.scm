; SICP Exercise 1.11, recursive and iterative processes

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(define (f-new n)
  (define (iter a b c n)
    (cond ((= n 0) c)
          (else (iter (+ a
                         (* 2 b)
                         (* 3 c))
                      a
                      b
                      (- n 1)))))
  (iter 2 1 0 n))
