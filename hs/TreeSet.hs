-- Sets as binary trees, page 210

module TreeSet
  ( Tree(..)
  , elementOfSet
  , adjoinSet
  , treeToList1
  , treeToList2
  , listToTree
  , unionSet
  , intersectionSet
  ) where

-- slightly different from Tree.hs, here nodes can have children and their own
-- elements
data Tree a = Tree a (Tree a) (Tree a) | Nil deriving (Eq, Show)

elementOfSet :: Ord a => a -> Tree a -> Bool
elementOfSet x s =
  case s of
    Nil -> False
    Tree e l r | x == e    -> True
               | x <  e    -> elementOfSet x l
               | otherwise -> elementOfSet x r

adjoinSet :: Ord a => a -> Tree a -> Tree a
adjoinSet x s =
  case s of
    Nil -> Tree x Nil Nil
    Tree e l r | x == e    -> s
               | x <  e    -> Tree e (adjoinSet x l) r
               | otherwise -> Tree e l               (adjoinSet x r)

-- Exercise 2.63, page 213
treeToList1 :: Tree a -> [a]
treeToList1 s =
  case s of
    Nil -> []
    Tree e l r -> treeToList1 l ++ e : treeToList1 r

treeToList2 :: Tree a -> [a]
treeToList2 s = copyToList s [] where
  copyToList tree result =
    case tree of
      Nil -> result
      Tree e l r -> copyToList l (e : copyToList r result)

-- part a
-- They are both in-order traversals of the tree, so they will both produce the
-- same result list for the same input tree.
--
-- part b
-- tree-to-list-1 uses append on the result of the left recursive call, so it
-- will use a quadratic number of steps in the size of the left subtree of the
-- root to convert the result to a list, because it does O(n) work in each
-- recursive call, and there are O(n) such calls.
-- tree-to-list-2 does only O(1) work in each recursive call, and there are O(n)
-- such calls, so tree-to-list-2 is O(n)

-- Exercise 2.64, page 215

listToTree :: [a] -> Tree a
listToTree elts = fst (partialTree elts (length elts))

partialTree :: [a] -> Int -> (Tree a, [a])
partialTree elts n =
  if n == 0
    then (Nil, elts)
    else (Tree thisEntry leftTree rightTree, remainingElts)
      where
        leftSize = quot (n - 1) 2
        rightSize = n - (leftSize + 1)
        (leftTree, nonLeftElts) = partialTree elts leftSize
        thisEntry = head nonLeftElts
        (rightTree, remainingElts) = partialTree (tail nonLeftElts) rightSize
-- part a
-- It first decides how many of the n elements to put into a left subtree, then
-- calls itself to produce this tree and the list of remaining elements. It
-- decides how many elements to put in the right subtree, as n minus the size of
-- the left subtree, minus 1 (for the root). It takes the first of the
-- non-left-elts for the root of the resulting partial tree, then calls itself
-- to produce the right subtree with the appropriate number of elements. It
-- returns both the tree formed from the root, left subtree, and right subtree,
-- as well as the remaining elements not consumed by the second recursive call.
--
-- part b
-- The work done in partial-tree excluding the recursive calls is O(1), and there
-- will be O(n) total calls to partial-tree altogether. Every call made with n >
-- 0 will consume a different element in the tree for its root, so there are n
-- such calls, and each call with n > 0 can make at most two calls with n = 0.
-- Therefore list-to-tree is O(n).

-- Exercise 2.65, page 216

unionSet :: Ord a => Tree a -> Tree a -> Tree a
unionSet x y = listToTree (merge (treeToList2 x) (treeToList2 y))
  where merge [] b  = b
        merge a  [] = a
        merge al@(a:as) bl@(b:bs) | a <  b    = a : merge as bl
                                  | a == b    = a : merge as bs
                                  | otherwise = b : merge al bs

intersectionSet :: Ord a => Tree a -> Tree a -> Tree a
intersectionSet x y = listToTree (merge (treeToList2 x) (treeToList2 y))
  where merge [] _  = []
        merge _  [] = []
        merge al@(a:as) bl@(b:bs) | a == b    = a : merge as bs
                                  | a <  b    = merge as bl
                                  | otherwise = merge al bs
