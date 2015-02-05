; Nested Mappings, page 166

(include "sequence.rkt")
(include "prime.rkt")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)         ; empty set?
    (list '())          ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; Exercise 2.40, page 169
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; Exercise 2.41, page 169
(define (unique-triples n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                    (map (lambda (i) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- k 1))))
           (enumerate-interval 1 n)))

(define (triple-sum-pairs n s)
  (filter (lambda (seq) (= s (apply + seq)))
          (unique-triples n)))

; Exercise 2.42, page 169
(define empty-board '())

(define (adjoin-position row k rest)
  (append rest (list (list row k))))

(define (safe? k positions)
  (let ((posnok (filter (lambda (p) (not (= k (cadr p)))) positions))
        (r (caar (filter (lambda (p) (= k (cadr p))) positions))))
    (null? (filter (lambda (p)
                     (let ((rp (car p)) (kp (cadr p)))
                     (or (= r rp)
                         (= k kp)
                         (= (abs (- r rp)) (abs (- k kp))))))
                   posnok))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Exercise 2.43, page 172
; for each potential new row, it attempts to re-arrange previous rows' queens
; such that the current queen's position is not in check, rather than choosing
; the current queen's position such that it is not in check given the previous
; queen's placements
; each call to queen-cols n generates board-size calls to queen-cols (n - 1)
; so the growth is exponential with a branching factor of board-size, and
; initially n = board-size, so this algorithm has complexity at least
; O((board-size)^(board-size))
; ignoring the work done in the map and flatmap
; TODO: make this a bit more rigorous
