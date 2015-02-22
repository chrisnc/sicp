{-# LANGUAGE FlexibleInstances #-}

-- Exercise 2.6, Church numerals, page 126

module ChurchNumerals
  ( zero
  , one
  , add
  , mul
  ) where

type ChurchNumeral a = (a -> a) -> a -> a

asChurch :: ChurchNumeral a -> ChurchNumeral a
asChurch = id

instance Num (ChurchNumeral a) where
  (+) a b f = a f . b f
  (*) = (.)
  (-) = error "cannot subtract Church numerals"
  negate = error "cannot negate Church numerals"
  abs = id
  signum = const one
  fromInteger n | n < 0 = error "cannot create negative Church numerals"
                | n == 0 = zero
                | even n = fromInteger (n `div` 2) * two
                | otherwise = fromInteger (n - 1) + one

--zero = \f -> \x -> x
zero :: ChurchNumeral a
zero f x = x

--addOne n = \f -> \x -> f ((n f) x)
--addOne n f x = f ((n f) x)
addOne :: ChurchNumeral a -> ChurchNumeral a
addOne n f = f . n f

-- implementing one
-- trivial definition
--one = addOne zero
-- substitute definition of addOne n (need to add f argument)
--one f = f . zero f
-- substitute zero f = id
--one f = f . id
-- identity law
one :: ChurchNumeral a
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
two :: ChurchNumeral a
two f = f . f
-- alternately
--two f x = f (f x)

-- implementing add
-- each numeral is applied to the procedure, each giving a procedure, which are
-- then applied to the argument x in succession
add :: ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
add a b f = a f . b f
-- alternately
--add a b f x = a f (b f x)

-- for fun, implement multiply
mul :: ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
mul a b = a . b
-- alternately
--mul a b f x = (a (b f)) x
--mul a b f = (a (b f))
--mul a b f = (a . b) f
--mul a b = a . b
