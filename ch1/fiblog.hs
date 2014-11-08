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

fib = iter 1 0 0 1 where
  iter a b p q n
    | n == 0    = b
    | even n    = iter a b (q^2 + p^2) (q^2 + 2*q*p) (n `div` 2)
    | otherwise = iter ((q + p)*a + q*b) (q*a + p*b) p q (n - 1)
