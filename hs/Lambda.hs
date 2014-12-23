-- Section 1.3.2, Constructing Procedures Using Lambda, page 83

module Lambda where

import SumProd

piSumLambda a b = 8 * sum' (\x -> 1.0 / (x * (x + 2))) (+ 4) a b

integral' f a b dx = dx * sum' f (+ dx) (a + (dx / 2.0)) b

fWithLet x y =
  let a = 1 + x * y
      b = 1 - y
  in x * a * a + y * b + a * b

square x = x * x

-- Exercise 1.34, page 88
f :: Num a => (a -> t) -> t
f g = g 2
-- f square
-- f (\z -> z * (z + 1))
-- f f
-- fails because of a type error
-- f :: Num a => (a -> t) -> t
-- so can't pass f to itself:
-- (a -> t) -> t
-- (a -> t) is not a Num

-- Section 1.3.3, Procedures as General Methods, page 89

closeEnough x y = abs (x - y) < 0.001

average x y = (x + y) / 2

search f negp posp =
  let mid = average negp posp
  in if closeEnough negp posp
    then mid
    else let testVal = f mid
             r | testVal > 0 = search f negp mid
               | testVal < 0 = search f mid posp
               | otherwise = mid
      in r

halfIntervalMethod f a b = r where
  aval = f a
  bval = f b
  r | aval < 0 && bval > 0 = search f a b
    | bval < 0 && aval > 0 = search f b a
    | otherwise =
        error $ "Values are not of opposite sign " ++ show a ++ " " ++ show b

tolerance = 0.00001

fixedPoint f firstGuess = try firstGuess where
  closeEnough v1 v2 = abs (v1 - v2) < tolerance
  try guess = let next = f guess in
    if closeEnough guess next then next else try next

sqrtFixed x = fixedPoint (\y -> average y (x / y)) 1.0

-- Exercise 1.35, page 94
-- x -> 1 + 1/x
-- x = 1 + 1/x
-- x^2 = x + 1
-- x^2 - x - 1 = 0
-- x = (1 + sqrt(1 + 4)) / 2
-- x = (1 + sqrt(5)) / 2 = phi

phi = fixedPoint (\x -> 1 + (1 / x)) 1.0

-- Exercise 1.36, page 94
fixedPointTrace f firstGuess = try firstGuess where
  closeEnough v1 v2 = abs (v1 - v2) < tolerance
  try guess = do
    putStrLn $ show guess
    let next = f guess
    if closeEnough guess next
      then return next
      else try next

runxxsol = do
  fixedPointTrace (\x -> log 1000 / log x) 2.0
  fixedPointTrace (\x -> (x + log 1000 / log x) / 2.0) 2.0

-- Exercise 1.37 (a), page 94
contFrac n d k = cfrack k 0 where
  cfrack i result =
    if i == 0
      then result
      else cfrack (i - 1) $ n i / (d i + result)

phiContFrac = contFrac (const 1.0) (const 1.0) 100

-- Exercise 1.37 (b), page 95
-- recursive process version
contFracRec n d k = cfrack 1 where
  cfrack i =
    if i == k
      then n i / d i
      else n i / (d i + cfrack (i + 1))

-- Exercise 1.38, page 96
eulerFrac k = contFrac (const 1.0) denom k where
  denom i =
    if rem (i - 2) 3 == 0
      then (fromIntegral i + 1) * 2 / 3
      else 1

-- Exercise 1.39, page 96
tanCf x k =
  contFrac
    (\i -> if i == 1 then x else negate (x * x))
    (\i -> fromIntegral i * 2 - 1)
    k

-- Section 1.3.4, Procedures as Returned Values, page 97
averageDamp f = \x -> average x (f x)

sqrtFixedAvg x = fixedPoint (averageDamp (x /)) 1.0

cubeRoot x = fixedPoint (averageDamp ((x /) . square)) 1.0

dx = 0.00001

deriv g x = (g (x + dx) - g x) / dx

newtonTransform g x = x - g x / (deriv g $ x)

newtonsMethod g = fixedPoint (newtonTransform g)

sqrtNewton x = newtonsMethod ((subtract x) . square) 1.0

fixedPointOfTransform g transform =
  fixedPoint (transform g)

sqrtFpotAvgDamp x = fixedPointOfTransform (x /) averageDamp 1.0

sqrtFpotNewton x = fixedPointOfTransform ((subtract x) . square) newtonTransform 1.0

-- Exercise 1.40, page 103
-- because of partial application, this is entirely equivalent to using lambda
cubic a b c x = x*x*x + a*x*x + b*x + c

-- Exercise 1.41, page 103
double f x = f (f x)

-- Exercise 1.42, page 103
compose f g x = f (g x)

-- Exercise 1.43, page 104
repeated f n = iter f n where
  iter fr n =
    if n == 1
      then fr
      else iter (compose f fr) (n - 1)

-- Exercise 1.44, page 104
smooth f x = (f (x - dx) + f x + f (x + dx)) / 3.0

smoothN f n = (repeated smooth n) f

-- Exercise 1.45, page 105
nthRoot n x =
  fixedPointOfTransform
    (\y -> x / (y ** (n - 1)))
    (repeated averageDamp 3)
    x

-- Exercise 1.46, page 105
iterativeImprove goodEnough improve = iter where
  iter x =
    if goodEnough x
      then x
      else iter (improve x)

sqrtImp x =
  iterativeImprove
    (\guess -> abs (square guess - x) < 0.001)
    (\guess -> average guess (x / guess))
    1.0

fixedPointImp f =
  iterativeImprove (\guess -> abs (guess - f guess) < tolerance) f

newtonsMethodImp g guess =
  fixedPointImp (newtonTransform g) guess
