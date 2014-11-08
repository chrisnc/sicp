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
