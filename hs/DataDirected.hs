-- Section 2.4.3, Data-Directed Programming and Additivity, page 242

{-# LANGUAGE FlexibleInstances #-}

module DataDirected
  ( Complex(..)
  , RectComplex
  , PolarComplex
  , convertComplex
  ) where

import GHC.Real (Ratio((:%)))

square :: Num a => a -> a
square x = x * x

class Complex c where
  real :: Floating a => c a -> a
  imag :: Floating a => c a -> a
  mag  :: Floating a => c a -> a
  ang  :: Floating a => c a -> a
  mkFromRealImag :: Floating a => a -> a -> c a
  mkFromMagAng   :: Floating a => a -> a -> c a

  -- default implementations for real and imag in terms of mag and ang
  real z = mag z * cos (ang z)
  imag z = mag z * sin (ang z)

  -- default implementations for mag and ang in terms of real and imag
  mag z = sqrt (square (real z) + square (imag z))

  -- TODO: need to fix this implementation, as atan will only give angles
  -- between -pi/2 and pi/2
  ang z = atan (imag z / real z)

  -- default implementation for mkFromRealImag in terms of mkFromMagAng
  mkFromRealImag x y = mkFromMagAng (sqrt (square x + square y)) (atan (y / x))

  -- default implementation for mkFromMagAng in terms of mkFromRealImag
  mkFromMagAng r a = mkFromRealImag (r * cos a) (r * sin a)

  -- MINIMAL pragma to require that instances override methods such that there
  -- is no circular dependency in the defaults.
  -- Logically, it would make sense to group mkFromRealImag with real and imag,
  -- but it is not strictly necessary because neither constructor calls real,
  -- imag, mag, or ang.
  {-# MINIMAL (real, imag | mag, ang), (mkFromRealImag | mkFromMagAng) #-}

data RectComplex a = RectComplex
  { rectReal :: a
  , rectImag :: a
  } deriving (Eq)

instance Show a => Show (RectComplex a) where
  show z = show (rectReal z) ++ " + " ++ show (rectImag z) ++ "i"

instance Complex RectComplex where
  real = rectReal
  imag = rectImag
  mkFromRealImag = RectComplex

data PolarComplex a = PolarComplex
  { polarMag :: a
  , polarAng :: a
  } deriving (Eq)

instance Show a => Show (PolarComplex a) where
  show z = show (polarMag z) ++ "e^(" ++ show (polarAng z) ++ "i)"

instance Complex PolarComplex where
  mag = polarMag
  ang = polarAng
  mkFromMagAng = PolarComplex

-- use an identical instance from hs/Complex.hs, except Complex
-- is now part of the context for the instance, rather than the type
instance (Complex c, Floating a) => Num (c a) where
  u + v = mkFromRealImag (real u + real v) (imag u + imag v)
  u * v = mkFromMagAng   (mag u * mag v)   (ang u + ang v)
  u - v = mkFromRealImag (real u - real v) (imag u - imag v)
  abs z = mkFromRealImag (mag z) 0
  signum z = z / abs z
  fromInteger n = mkFromRealImag (fromInteger n) 0

instance (Complex c, Floating a) => Fractional (c a) where
  u / v = mkFromMagAng (mag u / mag v) (ang u - ang v)
  fromRational (n :% d) = mkFromRealImag (fromInteger n / fromInteger d) 0

-- convert complex numbers from one representation to another
convertComplex :: (Complex c0, Complex c1, Floating a) => c0 a -> c1 a
convertComplex z = mkFromRealImag (real z) (imag z)
