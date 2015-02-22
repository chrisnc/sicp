-- Section 2.1.3, What is Meant by Data?, page 122

module Cons
  ( cons
  , car
  , cdr
  , cons'
  , car'
  , cdr'
  , makeIntPair
  , intPairFst
  , intPairSnd
  , zero
  , addOne
  , one
  , two
  , add
  , mul
  ) where

cons x y m
  | m == 0    = x
  | m == 1    = y
  | otherwise = error $ "Argument not 0 or 1: cons " ++ show m

car z = z 0
cdr z = z 1

-- Exercise 2.4, page 125
cons' x y m = m x y

car' z = z (\p q -> p)
cdr' z = z (\p q -> q)

-- Exercise 2.5, page 125
makeIntPair a b = 2 ^ a * 3 ^ b

intLogB b n = iter 0 n where
  iter r n =
    if rem n b /= 0
      then r
      else iter (r + 1) (n `div` b)


intPairFst p =
  if rem p 3 /= 0
    then intLogB 2 p
    else intPairFst (p `div` 3)

intPairSnd p =
  if rem p 2 /= 0
    then intLogB 3 p
    else intPairSnd (p `div` 2)
