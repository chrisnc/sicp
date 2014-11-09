-- SICP Exercise 1.17, multiplication by repeated addition, page 60

module Mul
  ( mul
  , fastmul
  , fastmul'
  ) where

mul a b =
  if b == 0
    then 0
    else a + (a * (b - 1))

double x = x + x
halve x = x `div` 2

fastmul a b
  | a == 1    = b
  | even a    = fastmul (halve a) (double b)
  | otherwise = b + (fastmul (a - 1) b)

-- Exercise 1.18, page 61
fastmul' = iter 0 where
  iter t a b
    | a == 0    = t
    | even a    = iter t (halve a) (double b)
    | otherwise = iter (t + b) (a - 1) b
