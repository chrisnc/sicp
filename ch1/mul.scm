; SICP Exercise 1.17, multiplication by repeated addition

(define (* a b)
  (if (= b 0)
    0
    (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast* a b)
  (cond ((= a 1) b)
        ((even? a) (fast* (halve a) (double b)))
        (else (+ b (fast* (- a 1) b)))))


; Exercise 1.18

(define (fastnew* a b)
  (define (iter a b t)
    (cond ((= a 0) t)
          ((even? a) (iter (halve a) (double b) t))
          (else (iter (- a 1) b (+ t b)))))
  (iter a b 0))
