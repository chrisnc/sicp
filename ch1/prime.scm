; SICP Section 1.2.6 Example, Testing for Primality

(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base ex m)
  (cond ((= ex 0) 1)
        ((even? ex)
         (remainder
           (square (expmod base (/ ex 2) m))
           m))
        (else
         (remainder
           (* base (expmod base (- ex 1) m))
           m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
