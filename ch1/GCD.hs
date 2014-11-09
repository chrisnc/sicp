-- SICP Section 1.2.5, Greatest Common Divisors, page 62

module GCD
  ( gcd'
  ) where

gcd' a b
  | b == 0    = a
  | otherwise = gcd' b (rem a b)
