-- Section 2.1.1, Example: Arithmetic Operations for Rational Numbers, page 113

module Rational
  ( Rat
  , makeRat
  ) where

import GHC.Real -- for fromRational

data Rat = Rat
  { numer :: Int
  , denom :: Int
  }

makeRat' n d = Rat (n `div` g) (d `div` g) where g = gcd n d

instance Num Rat where
  x + y = makeRat (numer x * denom y + numer y * denom x) (denom x * denom y)
  x - y = makeRat (numer x * denom y - numer y * denom x) (denom x * denom y)
  x * y = makeRat (numer x * numer y) (denom x * denom y)
  negate x = x { numer = negate (numer x) }
  abs x = x { numer = abs (numer x) }
  signum x | numer x > 0 = 1
           | numer x == 0 = 0
           | otherwise = (-1)
  fromInteger x = makeRat (fromIntegral x) 1

instance Fractional Rat where
  x / y = makeRat (numer x * denom y) (denom x * numer y)
  fromRational r =
    makeRat (fromIntegral (numerator r)) (fromIntegral (denominator r))

instance Eq Rat where
  x == y = numer x * denom y == numer y * denom x

instance Show Rat where
  show x = show (numer x) ++ "/" ++ show (denom x)

-- Exercise 2.1, page 118
makeRat n d =
  Rat ((if d < 0 then (- n) else n) `div` g) (abs d `div` g) where g = gcd n d
