{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercise 2.73, page 248

-- Using data-directed programming to implement a derivative routine.
-- To implement this in Haskell, we use typeclasses, which allow ad-hoc
-- extension, and type families, which maintain the type of expressions as they
-- are computed.

module DDDerivative
  ( Expr(..)
  , Sum(..)
  , Prod(..)
  , Exponent(..)
  , main
  ) where

import Data.String
import Data.List

main :: IO ()
main = do
  let e = Prod "x" (Exponent (Sum "x" (3 :: Double)) 4)
  print e
  print (deriv e "x")

-- an open type function to indecate the type of the derivative of an expression
-- of type e
type family Deriv e

-- an open class providing a derivative method
class Expr e where
  deriv :: e -> String -> Deriv e

-- implement deriv for the Double type
type instance Deriv Double = Double

instance Expr Double where
  deriv e _ = 0


-- implement deriv for String (used for variables)
type instance Deriv String = Double

instance Expr String where
  deriv e v = if e == v then 1 else 0


-- implement deriv for Sums
data Sum a b = Sum a b

instance (Show a, Show b) => Show (Sum a b) where
  show (Sum a b) = show a ++ " + " ++ show b

type instance Deriv (Sum a b) = Sum (Deriv a) (Deriv b)

instance (Expr a, Expr b) => Expr (Sum a b) where
  deriv (Sum a b) v = Sum (deriv a v) (deriv b v)


-- implement deriv for Products
data Prod a b = Prod a b

instance (Show a, Show b) => Show (Prod a b) where
  show (Prod a b) = "(" ++ show a ++ ") * (" ++ show b ++ ")"

type instance Deriv (Prod a b) = Sum (Prod a (Deriv b)) (Prod b (Deriv a))

instance (Expr a, Expr b) => Expr (Prod a b) where
  deriv (Prod a b) v = Sum (Prod a (deriv b v)) (Prod b (deriv a v))


-- implement deriv for Exponents
data Exponent a = Exponent a Integer

instance Show a => Show (Exponent a) where
  show (Exponent a n) = "(" ++ show a ++ ")^" ++ show n

type instance Deriv (Exponent a) = Prod Double (Prod (Exponent a) (Deriv a))

instance Expr a => Expr (Exponent a) where
  deriv (Exponent a n) v = Prod (fromIntegral n) (Prod (Exponent a (n - 1)) (deriv a v))
