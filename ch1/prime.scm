; SICP Section 1.2.6 Example, Testing for Primality

(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))

(define (next d)
  (cond ((= d 2) 3)
        (else (+ d 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (when (fast-prime? n 20)
    (report-prime (- (current-milliseconds) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lo hi)
  (when (<= lo hi)
    (begin
      (timed-prime-test lo)
      (search-for-primes (+ lo 1) hi))))

(define (expmodt base ex m)
  (cond ((= ex 0) 1)
        ((even? ex)
         (letrec ((z (expmodt base (/ ex 2) m))
                  (zsqm (remainder (square z) m)))
           (cond ((and (not (= z 1)) (not (= z (- m 1))) (= zsqm 1)) 0)
                 (else zsqm))
           ))
        (else
          (remainder
            (* base (expmod base (- ex 1) m))
            m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmodt a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else false)))
