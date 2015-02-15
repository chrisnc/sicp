-- Exercise 2.60, page 207

module MultiListSet
  ( elementOfSet
  , adjoinSet
  , intersectionSet
  , unionSet
  ) where

-- this is unchanged from the regular set case
-- O(n)
elementOfSet x s =
  case s of
    []           -> False
    e:_ | e == x -> True
    _:es         -> elementOfSet x es

-- just cons regardless of whether it exists in the set already
-- O(1)
adjoinSet = (:)

-- has to do the same thing as the regular set case
-- but this happens to not be commutative
-- (the number of duplicates depends only on what's in the first set)
-- O(n^2)
intersectionSet []     _  = []
intersectionSet _      [] = []
intersectionSet (x:xs) ys =
  if elementOfSet x ys
    then x : intersectionSet xs ys
    else intersectionSet xs ys

-- just stick them all together
-- O(n) where n is the length of the first argument
unionSet = (++)
