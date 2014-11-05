-- SICP Section 1.2.6 Example, Testing for Primality

import Control.Monad (replicateM)
import System.Random
import Data.Time.Clock

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

-- exercise 1.22
timedPrimeTest n = do
  putStr $ show n
  getCurrentTime >>= startPrimeTest n

startPrimeTest n startTime = do
  isprime <- fastPrimeIO 3 n
  if isprime
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

randomNumbers n = replicateM n . randomRIO

-- exercise 1.27
-- all fermatTestAll charmichael = True

fermatTestAll n = all (fermatTest n) [1..(n-1)]
charmichael = [561, 1105, 1729, 2465, 2821, 6601]

fastPrimeIO times n =
  fmap (all (fermatTest n)) $ randomNumbers times (1, n - 1)
