-- Section 2.4.1, Representations for Complex Numbers, page 232

module Complex
  (
  ) where

import GHC.Real (Ratio((:%)))

-- We use Haskell's built-in sum types to implement the type-tags described in
-- this section.
data Complex a = CRect a a | CPolar a a

makeFromRealImag :: a -> a -> Complex a
makeFromRealImag = CRect

makeFromMagAng :: a -> a -> Complex a
makeFromMagAng = CPolar

-- Pretty-printing for each representation of complex number.
instance Show a => Show (Complex a) where
  show z = case z of
    CRect x y -> show x ++ " + " ++ show y ++ "i"
    CPolar r a -> show r ++ "e^(" ++ show a ++ "i)"

-- These functions implement the dispatching to different implementations of
-- real, imag, mag, and ang based on the type tag. The implementations are far
-- too short to be worth separating into different functions, but one could do
-- this for other cases.
real :: Floating a => Complex a -> a
real z = case z of
  CRect  x _ -> x
  CPolar r a -> r * cos a

imag :: Floating a => Complex a -> a
imag z = case z of
  CRect  _ y -> y
  CPolar r a -> r * sin a

mag :: Floating a => Complex a -> a
mag z = case z of
  CRect  x y -> sqrt (x * x + y * y)
  CPolar r _ -> r

ang :: Floating a => Complex a -> a
ang z = case z of
  CRect  x y -> atan (y / x)
  CPolar _ a -> a

-- Using the implementations of real, imag, mag, and ang, we can implement
-- the Num and Fractional interfaces for our complex number type.
instance Floating a => Num (Complex a) where
  u + v = makeFromRealImag (real u + real v) (imag u + imag v)
  u * v = makeFromMagAng   (mag u * mag v)   (ang u + ang v)
  u - v = makeFromRealImag (real u - real v) (imag u - imag v)
  abs z = makeFromRealImag (mag z) 0
  signum z = z / abs z
  fromInteger n = makeFromRealImag (fromInteger n) 0

instance Floating a => Fractional (Complex a) where
  u / v = makeFromMagAng (mag u / mag v) (ang u - ang v)
  fromRational (n :% d) = makeFromRealImag (fromInteger n / fromInteger d) 0
