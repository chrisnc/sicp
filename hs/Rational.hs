-- Section 2.1.1, Example: Arithmetic Operations for Rational Numbers, page 113

module Rational
  ( Rat
  , makeRat
  ) where

import qualified GHC.Real as GHCR

data Ratio a = Ratio
  { numer :: a
  , denom :: a
  }

type Rat = Ratio Integer

makeRat' n d = Ratio (n `div` g) (d `div` g) where g = gcd n d

instance Integral a => Num (Ratio a) where
  x + y = makeRat (numer x * denom y + numer y * denom x) (denom x * denom y)
  x - y = makeRat (numer x * denom y - numer y * denom x) (denom x * denom y)
  x * y = makeRat (numer x * numer y) (denom x * denom y)
  negate x = x { numer = negate (numer x) }
  abs x = x { numer = abs (numer x) }
  signum x | numer x > 0 = 1
           | numer x == 0 = 0
           | otherwise = (-1)
  fromInteger x = makeRat (fromIntegral x) 1

instance Integral a => Fractional (Ratio a) where
  x / y = makeRat (numer x * denom y) (denom x * numer y)
  fromRational r =
    makeRat (fromIntegral (GHCR.numerator r)) (fromIntegral (GHCR.denominator r))

instance (Num a, Eq a) => Eq (Ratio a) where
  x == y = numer x * denom y == numer y * denom x

instance Show a => Show (Ratio a) where
  show x = show (numer x) ++ "/" ++ show (denom x)

-- Exercise 2.1, page 118
makeRat n d =
  Ratio ((if d < 0 then (- n) else n) `div` g) (abs d `div` g) where g = gcd n d
