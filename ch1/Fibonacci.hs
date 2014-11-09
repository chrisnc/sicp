-- SICP Section 1.2.2 example, Fibonacci numbers, page 47

module Fibonacci
  ( fib
  , fib'
  , fib''
  ) where

fib :: (Eq a, Num a) => a -> a
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fib' :: (Eq a, Num a) => a -> a
fib' = iter 1 0 where
  iter a b n
    | n == 0 = b
    | otherwise  = iter (a + b) a (n - 1)

-- SICP Exercise 1.11, recursive and iterative processes, page 53
f :: (Ord a, Num a) => a -> a
f n
  | n < 3     = n
  | otherwise = f (n - 1) + 2 * f (n - 2) + 3 * f (n - 3)

f' :: (Ord a, Num a) => a -> a
f' = iter 2 1 0 where
  iter a b c n
    | n == 0    = c
    | otherwise = iter (a + 2*b + 3*c) a b (n - 1)


-- SICP Exercise 1.19, Fibonacci numbers in logarithmic time, page 61

{--

 a' <- (q + p)a + qb
 b' <- qa + pb

 a'' <- (q + p)a' + qb'
 b'' <- qa' + pb'

 a'' <- (q + p)((q + p)a + qb) + q(qa + pb)
 b'' <- q((q + p)a + qb) + p(qa + pb)

 a'' <- (2q^2 + 2qp + p^2) a + (q^2 + 2qp)b
 b'' <- (q^2 + 2qp)a + (q^2 + p^2)b

 p' = q^2 + p^2
 q' = q^2 + 2qp

--}

fib'' = iter 1 0 0 1 where
  iter a b p q n
    | n == 0    = b
    | even n    = iter a b (q^2 + p^2) (q^2 + 2*q*p) (n `div` 2)
    | otherwise = iter ((q + p)*a + q*b) (q*a + p*b) p q (n - 1)
