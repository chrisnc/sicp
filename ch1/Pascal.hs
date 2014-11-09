-- SICP Exercise 1.12, Pascal's triangle, page 53

module Pascal
  ( pascal
  , pascalTriangle
  , showPascal
  ) where

-- generate an individual element
pascal r c
  | c >= r || c < 0 = 0
  | r == 1          = 1
  | otherwise       =
    (pascal (r - 1) c) + (pascal (r - 1) (c - 1))

-- version of zipWith that takes a default to use if either of the
-- lists runs out of elements before the other
zipWithDefault :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWithDefault f d (x:xs) (y:ys) = f x y : zipWithDefault f d xs ys
zipWithDefault f d []     (y:ys) = f d y : zipWithDefault f d [] ys
zipWithDefault f d (x:xs) []     = f x d : zipWithDefault f d xs []
zipWithDefault _ _ _      _      = []

-- produces the Pascal's Triangle up to row n
pascalTriangle :: (Num a, Eq a) => a -> [[a]]
pascalTriangle = iter [1] [] where
  iter l ls n
    | n == 0    = reverse ls
    | otherwise =
        iter (zipWithDefault (+) 0 l (0:l)) (l:ls) (n - 1)

showPascal :: Int -> IO ()
showPascal = mapM_ (putStrLn . show) . pascalTriangle
