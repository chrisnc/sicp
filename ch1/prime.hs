-- SICP Section 1.2.6 Example, Testing for Primality

import Control.Monad (replicateM)
import System.Random

square x = x * x

smallestDivisor n = findDivisor n 2

findDivisor n testDivisor
  | square testDivisor > n = n
  | divides testDivisor n  = testDivisor
  | otherwise              = findDivisor n (testDivisor + 1)

divides a b = rem b a == 0

prime n = n == smallestDivisor n

expmod base exp m
  | exp == 0  = 1
  | even exp  = rem (square (expmod base (exp `div` 2) m)) m
  | otherwise = rem (base * (expmod base (exp - 1) m)) m

fermatTest n a = expmod a n n == a

randomNumbers n = replicateM n . randomRIO

fastPrimeIO times n =
  fmap (all (fermatTest n)) $ randomNumbers times (1, n - 1)
