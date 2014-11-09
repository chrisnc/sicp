; Section 1.2.2 example, Fibonacci numbers, page 47

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-new n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; SICP Exercise 1.11, recursive and iterative processes, page 53
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

; SICP Exercise 1.19, Fibonacci numbers in logarithmic time, page 61

; a' <- (q + p)a + qb
; b' <- qa + pb
;
; a'' <- (2q^2 + 2qp + p^2) a + (q^2 + 2qp)b
; b'' <- (q^2 + 2qp)a + (q^2 + p^2)b

; p' = q^2 + p^2
; q' = q^2 + 2qp

(define (fib-log n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* q q) (* p p))   ; p'
                 (+ (* q q) (* 2 q p)) ; q'
                 (/ count 2)))
          (else (iter (+ (* (+ q p) a) (* q b))
                      (+ (* q a) (* p b))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))
