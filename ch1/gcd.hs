-- SICP Section 1.2.5, Greatest Common Divisors, page 62

mygcd a b
  | b == 0    = a
  | otherwise = mygcd b (rem a b)
