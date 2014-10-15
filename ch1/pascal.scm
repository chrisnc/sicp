; SICP Exercise 1.12, Pascal's triangle

(define (pascal r c)
  (cond ((or (>= c r) (< c 0)) 0)
        ((= r 1) 1)
        (else (+ (pascal (- r 1) c)
                 (pascal (- r 1) (- c 1))))))
