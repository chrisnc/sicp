; Section 2.3, Symbolic Data, page 192

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Exercise 2.53, page 195

(list 'a 'b 'c)
; '(a b c)
(list (list 'george))
; '((george))
(cdr '((x1 x2) (y1 y2)))
; '((y1 y2))
(cadr '((x1 x2) (y1 y2)))
; '(y1 y2)
(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; #f
(memq 'red '(red shoes blue socks))
;'(red shoes blue socks)


; Exercise 2.54, page 196
(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else #f)))

; Exercise 2.55, page 196
(car ''abracadabra)
; this expands to
(car (quote 'abracadabra))
; which expands to
(car (quote (quote abracadabra)))
; rewritten again
(car '(quote abracadabra))
; now it's obvious that the car of '(quote abracadabra) is 'quote
