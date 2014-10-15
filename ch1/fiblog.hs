-- SICP Exercise 1.19, Fibonacci numbers in logarithmic time

{--

 a' <- (q + p)a + qb
 b' <- qa + pb

 a'' <- (2q^2 + 2qp + p^2) a + (q^2 + 2qp)b
 b'' <- (q^2 + 2qp)a + (q^2 + p^2)b

 p' = q^2 + p^2
 q' = q^2 + 2qp

--}

fib = iter 1 0 0 1 where
  iter a b p q count
    | count == 0 = b
    | even count =
        iter a b (q^2 + p^2) (q^2 + 2*q*p) (count `div` 2)
    | otherwise  =
        iter ((q + p)*a + q*b) (q*a + p*b) p q (count - 1)
