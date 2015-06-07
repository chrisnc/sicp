-- Exercise 2.75, complex number constructor in message passing style page 253

module MPComplex
  ( ComplexOp(..)
  , makeFromRealImag
  , makeFromMagAng
  )
  where

data ComplexOp = RealPart | ImagPart | Magnitude | Angle

-- Example from page 252
makeFromRealImag :: RealFloat a => a -> a -> ComplexOp -> a
makeFromRealImag x y = dispatch where
  dispatch RealPart = x
  dispatch ImagPart = y
  dispatch Magnitude = sqrt (x * x + y * y)
  dispatch Angle = atan2 y x

makeFromMagAng :: RealFloat a => a -> a -> ComplexOp -> a
makeFromMagAng r a = dispatch where
  dispatch RealPart = r * cos a
  dispatch ImagPart = r * sin a
  dispatch Magnitude = r
  dispatch Angle = a
