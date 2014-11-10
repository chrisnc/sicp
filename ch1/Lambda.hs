-- Section 1.3.2, Constructing Procedures Using Lambda, page 83

module Lambda where

import SumProd

piSumLambda a b = 8 * sum' (\x -> 1.0 / (x * (x + 2))) (+ 4) a b

integral' f a b dx = dx * sum' f (+ dx) (a + (dx / 2.0)) b

fWithLet x y =
  let a = 1 + x * y
      b = 1 - y
  in x * a * a + y * b + a * b

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

search f negp posp =
  let mid = (negp + posp) / 2
  in if closeEnough negp posp
    then mid
    else let testVal = f mid
             r | testVal > 0 = search f negp mid
               | testVal < 0 = search f mid posp
               | otherwise = mid
      in r
