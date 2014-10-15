; SICP Section 1.2.4, Exponentiation

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define (expt-new b n)
  (define (iter n p)
    (if (= n 0)
      p
      (iter (- n 1) (* b p))))
  (iter n 1))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Exercise 1.16

(define (fast-expt-new b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))
