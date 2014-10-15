; SICP Section 1.2.5, Greatest Common Divisors

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))


; applicative order
(gcd 206 40)
(gcd 40 6) ; remainder 206 40
(gcd 6 4)  ; remainder 40 6
(gcd 4 2)  ; remainder 6 4
(gcd 2 0)  ; remainder 4 2
2

; normal order
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))

; etc., etc., ...
