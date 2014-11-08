-- SICP Section 1.2.4, Exponentiation, page 57

expt b n =
  if n == 0
    then 1
    else b * (expt b (n - 1))

exptnew b = iter 1 where
  iter p n
    | n == 0    = p
    | otherwise = iter (b * p) (n - 1)

square x = x * x

fastexpt b n
  | n == 0    = 1
  | even n    = square $ fastexpt b $ n `div` 2
  | otherwise = b * (fastexpt b (n - 1))


-- Exercise 1.16

fastexptnew = iter 1 where
  iter a b n
    | n == 0    = a
    | even n    = iter a (square b) (n `div` 2)
    | otherwise = iter (a * b) b (n - 1)
