-- Extended Exercise: Interval Arithmetic, page 126

module Interval
  ( Interval(..)
  , addInterval
  , mulInterval
  , divInterval
  , subInterval
  , intervalWidth
  , divInterval'
  , mulInterval'
  , makeCenterPercent
  , intervalCenter
  , percent
  , par1
  , par2
  ) where

-- Exercise 2.7, page 128
data Interval = Interval
  { lowerBound :: Double
  , upperBound :: Double
  }

instance Show Interval where
  show x = "(" ++ show (lowerBound x) ++ "," ++ show (upperBound x) ++ ")"

addInterval x y =
  Interval (lowerBound x + lowerBound y) (upperBound x + upperBound y)

mulInterval x y =
  Interval (minimum [p1, p2, p3, p4]) (maximum [p1, p2, p3, p4]) where
    p1 = lowerBound x * lowerBound y
    p2 = lowerBound x * upperBound y
    p3 = upperBound x * lowerBound y
    p4 = upperBound x * upperBound y

divInterval x y =
  mulInterval x $ Interval (1.0 / upperBound y) (1.0 / lowerBound y)

-- Exercise 2.8, page 128
subInterval x y =
  Interval (lowerBound x - upperBound y) (upperBound x - lowerBound y)

-- Exercise 2.9, page 128
intervalWidth x = (upperBound x - lowerBound x) / 2.0

-- let x and y be intervals, and lx, ly, ux, uy be the lower and upper bounds

-- for addition
-- x + y = (lx + ly, ux + uy)
-- width(x) = (ux - lx)/2
-- width(y) = (uy - ly)/2
-- width(x + y) = ((ux + uy) - (lx + ly))/2
-- width(x + y) = (ux + uy - lx - ly)/2
-- width(x + y) = (ux - lx)/2 + (uy - ly)/2
-- width(x + y) = width(x) + width(y)
--
-- for subtraction
-- x - y = (lx - uy, ux - ly)
-- width(x - y) = ((ux - ly) - (lx - uy))/2
-- width(x - y) = (ux - ly - lx + uy)/2
-- width(x - y) = (ux - lx + uy - ly)/2
-- width(x - y) = (ux - lx)/2 + (uy - ly)/2
-- width(x - y) = width(x) - width(y)
--
-- for multiplication
-- intervals: (1,2) * (3,4) = (3,8)
-- widths:    0.5   * 0.5   = 2.5
-- intervals: (2,3) * (3,4) = (6,12)
-- widths:    0.5   * 0.5   = 3
-- the input widths are the same, but the result widths are different
--
-- for division
-- intervals: (2,4) / (1,2) = (1,4)
-- widths:    1     / 0.5   = 1.5
-- intervals: (2,4) * (3,4) = (1/2,4/3)
-- widths:    1     * 0.5   = 5/12
-- the input widths are the same, but the result widths are different

-- Exercise 2.10, page 129
divInterval' x y =
  if lowerBound y < 0 && 0 < upperBound y
    then error $ "Denominator interval spans 0: " ++ show y
    else mulInterval x $ Interval (upperBound y / 1.0) (lowerBound y / 1.0)

-- Exercise 2.11, page 129
mulInterval' x y
  | lx > 0 && ux > 0 && ly < 0 && uy < 0 = Interval (ux * ly) (lx * uy)
  | lx < 0 && ux > 0 && ly < 0 && uy < 0 = Interval (ux * ly) (lx * ly)
  | lx < 0 && ux < 0 && ly < 0 && uy < 0 = Interval (ux * uy) (lx * ly)
  | lx < 0 && ux < 0 && ly < 0 && uy > 0 = Interval (lx * uy) (lx * ly)
  | lx < 0 && ux > 0 && ly < 0 && uy > 0 = Interval (min (lx * uy) (ux * ly)) (max (lx * ly) (ux * uy))
  | lx > 0 && ux > 0 && ly < 0 && uy > 0 = Interval (ux * ly) (ux * uy)
  | lx < 0 && ux < 0 && ly > 0 && uy > 0 = Interval (lx * uy) (ux * ly)
  | lx < 0 && ux > 0 && ly > 0 && uy > 0 = Interval (lx * uy) (ux * uy)
  | lx > 0 && ux > 0 && ly > 0 && uy > 0 = Interval (lx * ly) (ux * uy)
  where lx = lowerBound x
        ux = upperBound x
        ly = lowerBound y
        uy = upperBound y

-- Exercise 2.12, page 130
makeCenterPercent c p = Interval (c - w) (c + w) where
  w = c * p / 100

intervalCenter x = (lowerBound x + upperBound x) / 2.0

percent x =
  if c == 0
    then error "Interval center is zero, so percent tolerance is infinite."
    else 100 * w / c
  where
    w = intervalWidth x
    c = intervalCenter x

-- Exercise 2.13, page 130
-- If percentage tolerances are small, we have:
-- (x +/- d*x) * (y +/- e*y)
-- x*y +/- d*x*y +/- e*x*y +/- d*e*x*y
-- dropping small terms (d*e is much smaller than the other terms, if d ane e are
-- each small)
-- x*y +/- d*x*y +/- e*x*y
-- merging the +/- to reflect the lower and upper bound (either both + or both -)
-- x*y +/- (d + e)*x*y
-- We can see that d + e is just the percent error on the quantity x*y, the center
-- of the new interval.
-- So multiplying two intervals together will approximately add their percentage
-- uncertainties.

-- Exercise 2.14
par1 r1 r2 = divInterval (mulInterval r1 r2) (addInterval r1 r2)

par2 r1 r2 = divInterval one $ addInterval (divInterval one r1) (divInterval one r2)
  where one = Interval 1.0 1.0

a = Interval 3.99 4.01
b = Interval 4.99 5.01
--par1 a b
--par2 a b

-- Exercise 2.15
-- Essentially, repeating a value in an expression causes increased inaccuracy
-- because we cannot communicate to the interval arithmetic functions that the
-- uncertainty in two intervals in a compound expression is 100% correlated.
-- R1*R2 / (R1 + R2) is computed as though the first R1 is a different resistor
-- (with a possibly different actual resistance value) than the second R1, and
-- likewise for R2. When we only have one instance of each uncertain quantity in
-- the expression, this compounding of uncertainties does not occur. Eva is
-- correct; using expressions that do not repeat uncertain values will produce
-- tighter error bounds.

-- Exercise 2.16
-- Equivalent algebraic expressions may lead to different answers because we can
-- always cause a term to appear more than once, and our interval arithmetic
-- functions don't have any knowledge of correlated uncertainty, all
-- uncertainties in every term are implicitly independent.
-- We can only avoid doing this for functions where it is possible to write
-- interval-valued terms only once. Some functions cannot be simplified in this
-- way. Example: x / (x + y)
-- To do this properly would require evaluating the entire expression given all
-- pairs of upper and lower bounds of the interval variables, so duplicated
-- variables only ever take on one value at a time when evaluated.
