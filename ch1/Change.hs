-- SICP Counting Change example, page 51

module Change
  ( countChange
  ) where

coins :: Num a => [a]
coins = [1, 5, 10, 25, 50]

countChange :: (Num a, Ord a) => a -> a
countChange = cc coins where
  cc cs a
    | a == 0    = 1
    | null cs   = 0
    | a < 0     = 0
    | otherwise = (cc (tail cs) a) + (cc cs (a - head cs))
