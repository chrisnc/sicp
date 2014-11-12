; Exercise 2.2, page 121

(define (make-point x y) (cons x y))
(define (point-x p) (car p))
(define (point-y p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((a (start-segment s))
        (b (end-segment s)))
    (make-point
      (/ (+ (point-x a) (point-x b)) 2)
      (/ (+ (point-y a) (point-y b)) 2))))

(define (print-point p)
  (display "(")
  (display (point-x p))
  (display ",")
  (display (point-y p))
  (display ")"))

(define (print-segment s)
  (display "(")
  (print-point (start-segment s))
  (display ",")
  (print-point (end-segment s))
  (display ")"))

(define a (make-point 0 3))
(define b (make-point 4 9))
(define c (make-point 4 9))
(define d (make-point 4 9))
(define l (make-segment a b))

; Exercise 2.3, page 122

(define (make-rect width height center)
  (cons (cons width height) center))

(define (rect-width r) (car (car r)))
(define (rect-height r) (cdr (car r)))
(define (rect-center r) (cdr r))

(define (rect-area r) (* (rect-width r) (rect-height r)))
(define (rect-perimeter r) (* 2 (+ (rect-width r) (rect-height r))))

(define r1 (make-rect 4 5 (make-point 2 2.5)))
(display (rect-area r1))
(newline)
(display (rect-perimeter r1))
(newline)

; second representation
; assume the points a b c and d actually form a rectangle
; with a in the top left and going clockwise to d
(define (make-rect-from-points a b c d)
  (cons (cons a b) (cons c d)))

(define (rect-a r) (car (car r)))
(define (rect-b r) (cdr (car r)))
(define (rect-c r) (car (cdr r)))
(define (rect-d r) (cdr (cdr r)))

; get the center by averaging the four points
(define (rect-center r)
  (midpoint-segment
    (midpoint-segment
      (make-segment (rect-a r) (rect-b r)))
    (midpoint-segment
      (make-segment (rect-c r) (rect-d r)))))

(define (square x) (* x x))

(define (point-distance a b)
  (sqrt (+ (square (- (point-x a) (point-x b)))
           (square (- (point-y a) (point-y b))))))

; redefine rect-width and rect-height for this representation
(define (rect-width r)
  (point-distance (rect-a r) (rect-b r)))

(define (rect-height r)
  (point-distance (rect-a r) (rect-d r)))

; define the same rectangle as r1, but using the new representation
(define r2 (make-rect-from-points
             (make-point 0 5)
             (make-point 4 5)
             (make-point 4 0)
             (make-point 0 0)))

; demonstrate that rect-area and rect-perimeter (which were not redefined)
; still work on this new rect representation
(display (rect-area r2))
(newline)
(display (rect-perimeter r2))
(newline)
