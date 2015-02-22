; Section 2.1.3, What is Meant by Data?, 122

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: cons" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

; Exercise 2.4, page 125
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; Exercise 2.5, page 125
(define (make-int-pair a b)
  (* (expt 2 a) (expt 3 b)))

(define (int-log-b b n)
  (define (iter r n)
    (if (not (= (remainder n b) 0))
      r
      (iter (+ r 1) (/ n b))))
  (iter 0 n))

(define (int-pair-fst p)
  (if (not (= (remainder p 3) 0))
    (int-log-b 2 p)
    (int-pair-fst (/ p 3))))

(define (int-pair-snd p)
  (if (not (= (remainder p 2) 0))
    (int-log-b 3 p)
    (int-pair-snd (/ p 2))))

