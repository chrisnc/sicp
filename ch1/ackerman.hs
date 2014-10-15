-- SICP Exercise 1.10, the Ackerman function

ackerman :: (Eq a, Num a) => a -> a -> a
ackerman x y
  | y == 0    = 0
  | x == 0    = 2 * y
  | y == 1    = 2
  | otherwise = ackerman (x - 1) $ ackerman x (y - 1)
