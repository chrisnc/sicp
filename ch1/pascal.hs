-- SICP Exercise 1.12, Pascal's triangle

-- generate an individual element
pascal r c
  | c >= r || c < 0 = 0
  | r == 1          = 1
  | otherwise       =
    (pascal (r - 1) c) + (pascal (r - 1) (c - 1))

-- version of zipWith that takes a default to use if either of the
-- lists runs out of elements before the other
zipWithD :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWithD f d (x:xs) (y:ys) = f x y : zipWithD f d xs ys
zipWithD f d []     (y:ys) = f d y : zipWithD f d [] ys
zipWithD f d (x:xs) []     = f x d : zipWithD f d xs []
zipWithD _ _ _      _      = []

-- produces the Pascal's Triangle up to row n
pascal'sTriangle :: (Num a, Eq a) => a -> [[a]]
pascal'sTriangle = iter [1] [] where
  iter l ls n
    | n == 0    = reverse ls
    | otherwise =
        iter (zipWithD (+) 0 l (0:l)) (l:ls) (n - 1)

showPascal = mapM_ (putStrLn . show) . pascal'sTriangle
