-- Section 2.5.1, Generic Arithmetic Operations

-- This module encompasses exercises 2.77 through 2.86, pages 260-273

module DDNumbers
  ( Number(..)
  , i
  , cosine
  , sine
  , arctan
  , squareRoot
  , isZero
  , real
  , imag
  , mag
  , ang
  , towerRaise
  , towerDrop
  , towerProject
  ) where

import GHC.Real (Ratio(..))
import Data.Ratio
import Numeric (readFloat)

data Number =
    NInt Integer
  | NRat Rational
  | NReal Double
  | NCplx Complex

data Complex =
    CRect Number Number
  | CPolar Number Number

instance Show Number where
  show x = case x of
    NInt x        -> show x
    NRat (n :% d) -> show n ++ "/" ++ show d
    NReal x       -> show x
    NCplx x       -> show x

instance Show Complex where
  show z = case z of
    CRect  x y -> show x ++ " + " ++ show y ++ "i"
    CPolar r a -> show r ++ "e^(" ++ show a ++ "i)"

instance Eq Number where
  (==) = binaryTowerEq

real :: Number -> Number
real (NCplx (CRect a _)) = a
real (NCplx (CPolar r t)) = towerDrop (r * cosine t)
real x = x

imag :: Number -> Number
imag (NCplx (CRect _ b)) = b
imag (NCplx (CPolar r t)) = towerDrop (r * sine t)
imag x = 0

mag :: Number -> Number
mag (NCplx (CRect a b)) = squareRoot (a * a + b * b)
mag (NCplx (CPolar r _)) = r
mag x = x

ang :: Number -> Number
ang (NCplx (CRect a b)) = arctan b a
ang (NCplx (CPolar _ t)) = t
ang x = 0

numer :: Number -> Number
numer (NRat (n :% _)) = NInt n
numer x = x

denom :: Number -> Number
denom (NRat (_ :% d)) = NInt d
denom x = x

isZero :: Number -> Bool
isZero x = x == 0

data BinaryOp = Add | Sub | Mul | Div | Arctan

binaryTowerOp :: BinaryOp -> Number -> Number -> Number

binaryTowerOp Add (NInt x) (NInt y) = NInt (x + y)
binaryTowerOp Sub (NInt x) (NInt y) = NInt (x - y)
binaryTowerOp Mul (NInt x) (NInt y) = NInt (x * y)
binaryTowerOp Div (NInt x) (NInt y) = NRat (x :% y)

binaryTowerOp Add (NRat(xn :% xd)) (NRat(yn :% yd)) = NRat ((xn * yd + yn * xd) :% (xd * yd))
binaryTowerOp Sub (NRat(xn :% xd)) (NRat(yn :% yd)) = NRat ((xn * yd - yn * xd) :% (xd * yd))
binaryTowerOp Mul (NRat(xn :% xd)) (NRat(yn :% yd)) = NRat ((xn * yn) :% (xd * yd))
binaryTowerOp Div (NRat(xn :% xd)) (NRat(yn :% yd)) = NRat ((xn * yd) :% (xd * yn))

binaryTowerOp Add (NReal x) (NReal y) = NReal (x + y)
binaryTowerOp Sub (NReal x) (NReal y) = NReal (x - y)
binaryTowerOp Mul (NReal x) (NReal y) = NReal (x * y)
binaryTowerOp Div (NReal x) (NReal y) = NReal (x / y)

binaryTowerOp Add (x@NCplx{}) (y@NCplx{}) = NCplx (CRect (real x + real y) (imag x + imag y))
binaryTowerOp Sub (x@NCplx{}) (y@NCplx{}) = NCplx (CRect (real x - real y) (imag x - imag y))
binaryTowerOp Mul (x@NCplx{}) (y@NCplx{}) = NCplx (CPolar (mag x * mag y) (ang x + ang y))
binaryTowerOp Div (x@NCplx{}) (y@NCplx{}) = NCplx (CPolar (mag x / mag y) (ang x - ang y))

-- this will raise each operand until they are both NReal
binaryTowerOp Arctan (NReal x) (NReal y) = NReal (atan2 x y)
binaryTowerOp Arctan NCplx{} _ = error "arctan is not implemented for complex operands"
binaryTowerOp Arctan _ NCplx{} = error "arctan is not implemented for complex operands"

binaryTowerOp op x y | towerOrder x < towerOrder y = binaryTowerOp op (towerRaise x) y
binaryTowerOp op x y                               = binaryTowerOp op x (towerRaise y)

binaryTowerEq :: Number -> Number -> Bool

binaryTowerEq (NInt x) (NInt y) = x == y
binaryTowerEq (NRat(xn :% xd)) (NRat(yn :% yd)) = xn == yn && xd == yd
binaryTowerEq (NReal x) (NReal y)   = x == y
binaryTowerEq (x@NCplx{}) (y@NCplx{}) = real x == real y && imag x == imag y

binaryTowerEq x y | towerOrder x < towerOrder y = binaryTowerEq (towerRaise x) y
binaryTowerEq x y                               = binaryTowerEq x (towerRaise y)

towerNegate :: Number -> Number
towerNegate (NInt x) = NInt (negate x)
towerNegate (NRat x) = NRat (negate x)
towerNegate (NReal x) = NReal (negate x)
towerNegate (NCplx (CRect a b)) = NCplx (CRect (negate a) (negate b))
towerNegate (NCplx (CPolar r t)) = NCplx (CPolar (negate r) t)

towerAbs :: Number -> Number
towerAbs (NInt x) = NInt (abs x)
towerAbs (NRat x) = NRat (abs x)
towerAbs (NReal x) = NReal (abs x)
towerAbs (x@NCplx{}) = mag x

towerSignum :: Number -> Number
towerSignum (NInt x) = NInt (signum x)
towerSignum (NRat x) = NRat (signum x)
towerSignum (NReal x) = NReal (signum x)
towerSignum (x@NCplx{}) = x / mag x

towerOrder :: Number -> Int
towerOrder (NInt _)  = 0
towerOrder (NRat _)  = 1
towerOrder (NReal _) = 2
towerOrder (NCplx _) = 3

towerOrderMin, towerOrderMax :: Int
towerOrderMin = 0
towerOrderMax = 3

towerRaise :: Number -> Number
towerRaise (NInt x)        = NRat (x :% 1)
towerRaise (NRat (n :% d)) = NReal (fromIntegral n / fromIntegral d)
towerRaise x@NReal{}       = NCplx (CRect x 0)
towerRaise cplx            = cplx

-- for convenience of writing complex numbers, e.g., 3 + i 4
i :: Number -> Number
i x = NCplx (CRect 0 x)

towerProject :: Number -> Number
towerProject x@NCplx{} = real x + 0.0
towerProject (NReal x) = NRat (approxRational x (x * 1e-9))
towerProject (NRat (n :% d)) = NInt (div n d)
towerProject x = x

towerDrop :: Number -> Number
towerDrop x | towerOrder x == towerOrderMin = x
towerDrop x =
  if x == xd
    then towerDrop xd
    else x
  where xd = towerProject x

instance Num Number where
  x + y = towerDrop (binaryTowerOp Add x y)
  x - y = towerDrop (binaryTowerOp Sub x y)
  x * y = towerDrop (binaryTowerOp Mul x y)
  negate = towerDrop . towerNegate
  abs = towerDrop . towerAbs
  signum = towerDrop . towerSignum
  fromInteger = NInt

instance Fractional Number where
  x / y = towerDrop (binaryTowerOp Div x y)
  fromRational = NRat

isInteger :: Double -> Bool
isInteger x = x == fromIntegral (round x)

squareRoot :: Number -> Number
squareRoot (NInt x) = towerDrop (NReal (sqrt (fromIntegral x)))
squareRoot (NRat (n :% d)) = towerDrop (
  let sqtn = sqrt (fromIntegral n)
      sqtd = sqrt (fromIntegral d)
  in if isInteger sqtn && isInteger sqtd
    then NRat (round sqtn :% round sqtd)
    else NReal (sqtn / sqtd)
  )
squareRoot (NReal x) = towerDrop (NReal (sqrt x))
squareRoot x@NCplx{} = towerDrop (NCplx (CPolar (squareRoot (mag x)) (ang x / 2)))

sine :: Number -> Number
sine x@NInt{} = sine (towerRaise x)
sine x@NRat{} = sine (towerRaise x)
sine (NReal x) = towerDrop (NReal (sin x))
sine x@NCplx{} = error "sine of a complex number is not implemented"

cosine :: Number -> Number
cosine (NInt x) = towerDrop (NReal (cos (fromInteger x)))
cosine x@NRat{} = towerDrop (cosine (towerRaise x))
cosine (NReal x) = towerDrop (NReal (cos x))
cosine x@NCplx{} = error "cosine of a complex number is not implemented"

arctan :: Number -> Number -> Number
arctan = binaryTowerOp Arctan
