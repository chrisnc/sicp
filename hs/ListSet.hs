-- Example 2.3.3: Representing Sets, page 206

module ListSet
  ( elementOfSet
  , adjoinSet
  , intersectionSet
  , unionSet
  ) where

elementOfSet x s =
  case s of
    []           -> False
    e:_ | e == x -> True
    _:es         -> elementOfSet x es

adjoinSet x s =
  if elementOfSet x s
    then s
    else x:s

intersectionSet []     _  = []
intersectionSet _      [] = []
intersectionSet (x:xs) ys =
  if elementOfSet x ys
    then x : intersectionSet xs ys
    else intersectionSet xs ys

-- Exercise 2.59, page 207
unionSet []     ys = ys
unionSet xs     [] = xs
unionSet (x:xs) ys =
  if elementOfSet x ys
    then unionSet xs ys
    else x : unionSet xs ys
