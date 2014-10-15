-- SICP Section 1.2.5, Greatest Common Divisors

mygcd a b
  | b == 0    = a
  | otherwise = mygcd b (rem a b)
