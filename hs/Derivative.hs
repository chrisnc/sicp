-- Section 2.3.2, Example: Symbolic Differentiation, page 197

-- this module subsumes exercises 2.56, 2.57, and 2.58(a,b)
-- it implements symbolic differentiation with exponentiation
-- and infix notation with the correct operator precedence using the functions
-- from Num and a custom exponent operator (^*)
-- it will pretty-print results in normal mathematical notation
-- (but does not completely simplify, just simple cases)

-- example usage:
-- deriv ("x"^*5 + 2*"x") "x"
-- 5 * x^4 + 2

module Derivative
  ( Expr
  , mkSum
  , mkProd
  , mkExponent
  , deriv
  , (^*)
  , asExpr
  ) where

import Data.String
import Data.List

data Expr a b =
    Constant a
  | Var String
  | Sum (Expr a b) (Expr a b)
  | Prod (Expr a b) (Expr a b)
  | Exponent (Expr a b) b
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Expr a b) where
  show (Var v) = v
  show (Sum x y) = show x ++ " + " ++ show y
  show (Prod x y) = showp x ++ " * " ++ showp y
  show (Exponent e n) = showe e ++ "^" ++ show n
  show (Constant c) = show c

-- like show, but wraps sums in parentheses
-- (called by Prod's show)
showp :: (Show a, Show b) => Expr a b -> String
showp e = case e of
  Sum _ _ -> "(" ++ show e ++ ")"
  _       -> show e

-- like show, but wraps sums, products, and exponents in parentheses
-- (called by Exponent's show)
showe :: (Show a, Show b) => Expr a b -> String
showe e = case e of
  Sum _ _      -> "(" ++ show e ++ ")"
  Prod _ _     -> "(" ++ show e ++ ")"
  Exponent _ _ -> "(" ++ show e ++ ")"
  _            -> show e

instance (Eq a, Num a, Integral b) => Num (Expr a b) where
  (+)         = mkSum
  (*)         = mkProd
  negate      = mkProd (Constant (-1))
  abs         = error "abs not defined for symbolic expressions"
  signum      = error "signum not defined for symbolic expresssions"
  fromInteger = Constant . fromInteger

-- because (^), (^^), and (**) are already taken and don't really do what we
-- want. (only (**) is implemented in a typeclass, but it's Floating, which has
-- a bunch of other stuff, too)
infixr 8 ^*
(^*) :: (Num a, Integral b) => Expr a b -> b -> Expr a b
(^*) = mkExponent

-- TODO
{-
instance (Eq a, Num a, Integral b) => Fractional (Expr a b) where
  (/)          = undefined
  recip        = undefined
  fromRational = undefined
-}

-- this lets you create variables by just writing a String literal
-- using -XOverloadedStrings
instance Num a => IsString (Expr a b) where
  fromString = Var


-- TODO: can do a better job of simplifying, maybe make a 'simplify' function
-- that applies all transformations until fixed point

mkSum :: (Eq a, Num a, Integral b) => Expr a b -> Expr a b -> Expr a b
mkSum x             (Constant 0)   = x
mkSum (Constant 0)  y              = y
mkSum (Constant cx) (Constant cy)  = Constant (cx + cy)
mkSum x             y              | x == y = 2 * x
mkSum x             y              = Sum x y

mkProd :: (Eq a, Num a, Integral b) => Expr a b -> Expr a b -> Expr a b
mkProd _              (Constant 0)   = Constant 0
mkProd x              (Constant 1)   = x
mkProd (Constant 0)   _              = Constant 0
mkProd (Constant 1)   y              = y
mkProd (Constant cx)  (Constant cy)  = Constant (cx * cy)
mkProd (Exponent x a) y              | x == y = mkExponent x (a + 1)
mkProd x              (Exponent y a) | x == y = mkExponent x (a + 1)
mkProd (Exponent x a) (Exponent y b) | x == y = mkExponent x (a + b)
mkProd x              y              | x == y = mkExponent x 2
mkProd x              y              = Prod x y

mkExponent :: (Num a, Integral b) => Expr a b -> b -> Expr a b
mkExponent (Exponent x n0) n1 = Exponent x (n0 * n1)
mkExponent x 0                = Constant 1
mkExponent x 1                = x
mkExponent x n                = Exponent x n

-- simple way of forcing something in the repl to be interpreted as an
-- expression, without adding Expr to our default list
asExpr :: Expr a b -> Expr a b
asExpr = id

deriv :: (Eq a, Num a, Integral b) => Expr a b -> String -> Expr a b
deriv expr v =
  case expr of
    Sum x y        -> deriv x v + deriv y v
    Prod x y       -> x * deriv y v + y * deriv x v
    Exponent x n   -> fromIntegral n * x^*(n - 1) * deriv x v
    Var x | x == v -> 1
    Var _          -> 0
    Constant _     -> 0
