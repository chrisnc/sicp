-- Section 1.2.6 Example: Testing for Primality, page 65

module Prime
  ( prime
  )
  where

import Control.Monad (replicateM)
import System.Random
import Data.Time.Clock

square x = x * x

smallestDivisor n = findDivisor n 2

-- exercise 1.21, page 70
-- map smallestDivisor [199, 1999, 19999]

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

-- exercise 1.22, page 70
timedPrimeTest n = do
  putStr $ show n
  getCurrentTime >>= startPrimeTest n

startPrimeTest n startTime = do
-- exercise 1.24, page 72
  --isprime <- fastPrimeIO 3 n
  --if isprime
  if prime n
    then do
      resultTime <- getCurrentTime
      reportPrime $ diffUTCTime resultTime startTime
    else putStrLn ""

reportPrime elapsedTime =
  putStrLn $ " *** " ++ show elapsedTime

searchForPrimes :: Int -> Int -> IO ()
searchForPrimes lo hi =
  mapM_ timedPrimeTest [lo..hi]

main = searchForPrimes 2 1000000

-- exercise 1.23, page 71
findDivisor' n testDivisor
  | square testDivisor > n = n
  | divides testDivisor n  = testDivisor
  | otherwise              = findDivisor n $ nextDivisor testDivisor

smallestDivisor' n = findDivisor' n 2

nextDivisor n = if n == 2 then 3 else n + 2

fastPrimeIO times n =
  fmap (all (fermatTest n)) $ randomNumbers times (1, n - 1)

randomNumbers n = replicateM n . randomRIO

-- exercise 1.27, page 73
-- all fermatTestAll charmichael = True

fermatTestAll n = all (fermatTest n) [1..(n-1)]
charmichael = [561, 1105, 1729, 2465, 2821, 6601]

fastFermatPrimeIO times n =
  fmap (all (fermatTest n)) $ randomNumbers times (1, n - 1)

-- exercise 1.28, page 73
expmod' base exp m
  | exp == 0  = 1
  | even exp  =
    let z = expmod' base (exp `div` 2) m
        zsqm = rem (square z) m
    in if z /= 1 && z /= m - 1 && zsqm == 1
      then 0
      else zsqm
  | otherwise = rem (base * (expmod' base (exp - 1) m)) m

millerRabinTest n a = expmod' a (n - 1) n == 1

millerRabinIO times n =
  fmap (all (millerRabinTest n)) $ randomNumbers times (1, n - 1)
