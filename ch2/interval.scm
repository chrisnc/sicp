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

; Exercise 2.10, page 129
(define (div-interval x y)
  (if (and (negative? (lower-bound y)) (positive? (upper-bound y)))
    (error "Dividing by an interval that spans 0:" (lower-bound y) (upper-bound y))
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

; Exercise 2.11, page 129
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (positive? lx) (positive? ux) (negative? ly) (negative? uy))
           (make-interval (* ux ly) (* lx uy)))
          ((and (negative? lx) (positive? ux) (negative? ly) (negative? uy))
           (make-interval (* ux ly) (* lx ly)))
          ((and (negative? lx) (negative? ux) (negative? ly) (negative? uy))
           (make-interval (* ux uy) (* lx ly)))
          ((and (negative? lx) (negative? ux) (negative? ly) (positive? uy))
           (make-interval (* lx uy) (* lx ly)))
          ((and (negative? lx) (positive? ux) (negative? ly) (positive? uy))
           (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
          ((and (positive? lx) (positive? ux) (negative? ly) (positive? uy))
           (make-interval (* ux ly) (* ux uy)))
          ((and (negative? lx) (negative? ux) (positive? ly) (positive? uy))
           (make-interval (* lx uy) (* ux ly)))
          ((and (negative? lx) (positive? ux) (positive? ly) (positive? uy))
           (make-interval (* lx uy) (* ux uy)))
          ((and (positive? lx) (positive? ux) (positive? ly) (positive? uy))
           (make-interval (* lx ly) (* ux uy))))))

; Exercise 2.12, page 130
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w) (+ c w))))

(define (interval-center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percent x)
  (let ((w (interval-width x))
        (c (interval-center x)))
    (if (= c 0)
      (error "Interval center is zero, so percent tolerance is infinite.")
      (* 100 (/ w c)))))

; Exercise 2.13, page 130
; If percentage tolerances are small, we have:
; (x +/- d*x) * (y +/- e*y)
; x*y +/- d*x*y +/- e*x*y +/- d*e*x*y
; dropping small terms (d*e is much smaller than the other terms, if d ane e are
; each small)
; x*y +/- d*x*y +/- e*x*y
; merging the +/- to reflect the lower and upper bound (either both + or both -)
; x*y +/- (d + e)*x*y
; We can see that d + e is just the percent error on the quantity x*y, the center
; of the new interval.
; So multiplying two intervals together will approximately add their percentage
; uncertainties.

; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

(define a (make-interval 3.99 4.01))
(define b (make-interval 4.99 5.01))
(par1 a b)
(par2 a b)

; Exercise 2.15
; Essentially, repeating a value in an expression causes increased inaccuracy
; because we cannot communicate to the interval arithmetic functions that the
; uncertainty in two intervals in a compound expression is 100% correlated.
; R1*R2 / (R1 + R2) is computed as though the first R1 is a different resistor
; (with a possibly different actual resistance value) than the second R1, and
; likewise for R2. When we only have one instance of each uncertain quantity in
; the expression, this compounding of uncertainties does not occur. Eva is
; correct; using expressions that do not repeat uncertain values will produce
; tighter error bounds.

; Exercise 2.16
; Equivalent algebraic expressions may lead to different answers because we can
; always cause a term to appear more than once, and our interval arithmetic
; functions don't have any knowledge of correlated uncertainty, all
; uncertainties in every term are implicitly independent.
; We can only avoid doing this for functions where it is possible to write
; interval-valued terms only once. Some functions cannot be simplified in this
; way. Example: x / (x + y)
