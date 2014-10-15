-- SICP Section 1.2.2 example, Fibonacci numbers

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
