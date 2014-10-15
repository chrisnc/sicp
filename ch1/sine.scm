; SICP Exercise 1.15, sine of an angle

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine a)
  (if (not (> (abs a) 0.1))
    a
    (p (sine (/ angle 3.0)))))
