-- SICP Section 1.2.1 example, factorial, page 41

module Factorial
  ( factorial
  , factorial'
  ) where

factorial :: (Eq a, Num a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n - 1)

factorial' :: (Ord a, Num a) => a -> a
factorial' n = iter 1 1 where
  iter product counter =
    if counter > n
      then product
      else iter (counter * product) (counter + 1)
