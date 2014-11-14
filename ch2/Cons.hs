-- Section 2.1.3, What is Meant by Data?, 122

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

-- Exercise 2.6, page 126

--zero = \f -> \x -> x
zero f x = x

--addOne n = \f -> \x -> f ((n f) x)
--addOne n f x = f ((n f) x)
addOne n f = f . n f

-- implementing one
-- trivial definition
--one = addOne zero
-- substitute definition of addOne n (need to add f argument)
--one f = f . zero f
-- substitute zero f = id
--one f = f . id
-- identity law
one f = f
-- alternately
--one f x = f x

-- implementing two
-- trivial definition
--two = addOne one
--two f = addOne one f
-- substitute addOne n f = f . n f
--two f = f . one f
-- substitute one f = f
two f = f . f
-- alternately
--two f x = f (f x)

-- implementing add
-- each numeral is applied to the procedure, each giving a procedure, which are
-- then applied to the argument x in succession
add a b f = a f . b f
-- alternately
--add a b f x = a f (b f x)

-- for fun, implement multiply
mul a b = a . b
-- alternately
--mul a b f x = (a (b f)) x
