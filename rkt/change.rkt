; Counting Change example, page 51

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Challenge problem; iterative process version
; (This doesn't do any memoization, it just uses an explicit list to keep track
; of the (amount . kinds) pairs that are called. Each call to cc-loop pops
; off the head of the list and either consumes it or replaces it with two new
; pairs.
(define (cc-iter amount kinds-of-coins)
  (define (cc-loop poss n)
    (if (null? poss)
      n
      (let ((amnt (car (car poss)))
            (kinds (cdr (car poss))))
        (cond ((= amnt 0) (cc-loop (cdr poss) (+ n 1)))
              ((or (< amnt 0) (= kinds 0)) (cc-loop (cdr poss) n))
              (else (cc-loop (append (list (cons amnt
                                                 (- kinds 1))
                                           (cons (- amnt (first-denomination kinds))
                                                 kinds))
                                     (cdr poss))
                             n))))))
  (cc-loop (list (cons amount kinds-of-coins)) 0))
