-- Section 1.3, Formulating Abstractions with Higher-Order Procedures, page 74

module SumProd
  ( sumCubes
  , sumIntegers
  , piSum
  , integral
  , simpson
  , main
  , sum'
  , product'
  , piProd
  , product''
  , accumulate
  , sumacc
  , productacc
  , accumulaterec
  , filteredAccumulate
  , sumSqPrimes
  , prodRelPrime
  ) where

import Prime -- for exercise 1.33

mysum term next a b =
  if a > b
    then 0
    else term a + mysum term next (next a) b

mysum' term next a b =
  sum . map term . takeWhile (<= b) $ iterate next a

cube x = x * x * x

inc = (+ 1)

sumCubes = sum' cube inc

sumIntegers = sum' id inc

piSum = sum' piTerm piNext where
  piTerm x = 1.0 / (x * (x + 2))
  piNext = (+ 4)

integral f a b dx = dx * sum' f (+ dx) (a + dx / 2.0) b

-- Exercise 1.29: Simpson's Rule, page 80
simpson f a b n = h / 3 * sum' simpsonTerm inc 0 n where
  h = (b - a) / fromIntegral n
  y k = f $ a + fromIntegral k * h
  c k | k == 0 = 1
      | k == n = 1
      | even k = 2
      | otherwise = 4
  simpsonTerm k = c k * y k

main = putStrLn . show $ simpson cube 0 1 10000000

-- Exercise 1.30, page 80
sum' term next a b = iter a 0 where
  iter a result =
    if a > b
      then result
      else iter (next a) $ result + term a

-- Exercise 1.31 (a), page 80
product' term next a b = iter a 1 where
  iter a result =
    if a > b
      then result
      else iter (next a) $ result * term a

-- pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * ...
piProd n = 4 * product' piTerm piNext (2,3) (n,n+1) where
  piNext (a,b) = if a > b then (a, b + 2) else (a + 2, b)
  piTerm = uncurry (/)

-- Exercise 1.31 (b), page 81
-- recursive process version
product'' term next a b =
  if a > b
    then 1
    else term a * product'' term next (next a) b

-- Exercise 1.32 (a), page 81
-- use type signatures so sumacc and productacc can be written point-free
accumulate :: Ord a => (r -> b -> r) -> r -> (a -> b) -> (a -> a) -> a -> a -> r
accumulate combine nv term next a b = iter a nv where
  iter a result =
    if a > b
      then result
      else iter (next a) $ combine result $ term a

sumacc :: (Ord a, Num b) => (a -> b) -> (a -> a) -> a -> a -> b
sumacc = accumulate (+) 0

productacc :: (Ord a, Num b) => (a -> b) -> (a -> a) -> a -> a -> b
productacc = accumulate (*) 1

-- Exercise 1.32 (b), page 82
-- recursive process version
accumulaterec :: Ord a => (r -> b -> r) -> r -> (a -> b) -> (a -> a) -> a -> a -> r
accumulaterec combine nv term next a b =
  if a > b
    then nv
    else combine (accumulaterec combine nv term next (next a) b) (term a)

-- Exercise 1.33, page 82
filteredAccumulate combine nv term next filt a b = iter a nv where
  iter a result
    | a > b     = result
    | filt a    = iter (next a) $ combine result $ term a
    | otherwise = iter (next a) result

-- Exercise 1.33 (a), page 82
sumSqPrimes = filteredAccumulate (+) 0 (\x -> x*x) succ prime

-- Exercise 1.33 (b), page 82
prodRelPrime n = filteredAccumulate (*) 1 id succ ((== 1) . gcd n) 1 (n-1)
