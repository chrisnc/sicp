; Extended Exercise: Interval Arithmetic, page 126

; Exercise 2.7, page 128
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

; Exercise 2.8, page 128
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9, page 128
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

; let x and y be intervals, and lx, ly, ux, uy be the lower and upper bounds

; for addition
; x + y = (lx + ly, ux + uy)
; width(x) = (ux - lx)/2
; width(y) = (uy - ly)/2
; width(x + y) = ((ux + uy) - (lx + ly))/2
; width(x + y) = (ux + uy - lx - ly)/2
; width(x + y) = (ux - lx)/2 + (uy - ly)/2
; width(x + y) = width(x) + width(y)

; for subtraction
; x - y = (lx - uy, ux - ly)
; width(x - y) = ((ux - ly) - (lx - uy))/2
; width(x - y) = (ux - ly - lx + uy)/2
; width(x - y) = (ux - lx + uy - ly)/2
; width(x - y) = (ux - lx)/2 + (uy - ly)/2
; width(x - y) = width(x) - width(y)

; for multiplication
; intervals: (1,2) * (3,4) = (3,8)
; widths:    0.5   * 0.5   = 2.5
; intervals: (2,3) * (3,4) = (6,12)
; widths:    0.5   * 0.5   = 3
; the input widths are the same, but the result widths are different

; for division
; intervals: (2,4) / (1,2) = (1,4)
; widths:    1     / 0.5   = 1.5
; intervals: (2,4) * (3,4) = (1/2,4/3)
; widths:    1     * 0.5   = 5/12
; the input widths are the same, but the result widths are different
