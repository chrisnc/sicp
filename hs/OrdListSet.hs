module OrdListSet
  ( elementOfSet
  , intersectionSet
  , adjoinSet
  , unionSet
  ) where

elementOfSet :: Ord a => a -> [a] -> Bool
elementOfSet x s = case s of
  [] -> False
  e:_ | x == e -> True
  e:_ | x < e  -> False
  _:es         -> elementOfSet x es

intersectionSet :: Ord a => [a] -> [a] -> [a]
intersectionSet []        _                 = []
intersectionSet _         []                = []
intersectionSet xl@(x:xs) yl@(y:ys) | x == y    = x : intersectionSet xs ys
                                    | x <  y    = intersectionSet xs yl
                                    | otherwise = intersectionSet xl ys

-- Exercise 2.61, page 210
adjoinSet :: Ord a => a -> [a] -> [a]
adjoinSet x s = case s of
  [] -> [x]
  (e:_) | x < e  -> x : s
  (e:_) | x == e -> s
  (e:es)         -> e : adjoinSet x es

-- Exercise 2.62, page 210
unionSet :: Ord a => [a] -> [a] -> [a]
unionSet [] yl = yl
unionSet xl [] = xl
unionSet xl@(x:xs) yl@(y:ys) | x <  y    = x : unionSet xs yl
                             | x == y    = x : unionSet xs ys
                             | otherwise = y : unionSet xl ys
