-- Section 2.2.2, Hierarchical Structures, page 147

module Tree
  ( Tree(..)
  , list
  , cons
  , append
  , countLeaves
  , deepReverse
  , fringe
  , Mobile(..)
  , Branch(..)
  , Structure(..)
  , branchWeight
  , mobileWeight
  , balancedBranch
  , branchTorque
  , balancedMobile
  , scaleTree
  , scaleTree'
  , squareTree
  , squareTree'
  , treeMap
  , subsets
  ) where

data Tree a = Pair (Tree a) (Tree a) | Node a | Nil

cons = Pair

-- when the argument is already a list of Tree a
list = foldr cons Nil

-- when the argument is not already a list of Tree a
list' = foldr (cons . Node) Nil

instance Functor Tree where
  fmap f t = case t of
    Nil      -> Nil
    Node a   -> Node (f a)
    Pair a b -> Pair (fmap f a) (fmap f b)

append a b = case a of
  Nil      -> b
  Node _   -> cons a b
  Pair c d -> cons c (append d b)

instance Show a => Show (Tree a) where
  show t = case t of
    Nil      -> "()"
    Node a   -> show a
    Pair a b -> "(" ++ show a ++ showSublist b

showSublist t = case t of
  Nil      -> ")"
  Node a   -> " . " ++ show a ++ ")"
  Pair a b -> " " ++ show a ++ showSublist b

countLeaves t = case t of
  Nil      -> 0
  Node _   -> 1
  Pair a b -> countLeaves a + countLeaves b

-- Exercise 2.24, page 149
-- (list 1 (list 2 (list 3 4)))
l = cons
      (Node 1)
      (cons
        (cons
          (Node 2)
          (cons
            (cons
              (Node 3)
              (cons
                (Node 4)
                Nil))
            Nil))
        Nil)

-- result from interpreter
-- '(1 (2 (3 4)))

-- as a tree
--       .
--      / \
--     1   .
--        / \
--       2   .
--          / \
--         3   4

-- Exercise 2.25, page 149
--(display (cadr (caddr '(1 3 (5 7) 9))))
--(newline)
--(display (caar '((7))))
--(newline)
--(display (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))
--(newline)

-- Exercise 2.26, page 150
x = list' [1,2,3]
y = list' [1,2,3]

main = do
  putStrLn $ show $ append x y
  putStrLn $ show $ cons x y
  putStrLn $ show $ list [x, y]

-- Exercise 2.27, page 150
deepReverse l = iter Nil l where
  iter r l = case l of
    Nil                 -> r
    Node _              -> cons l r
    Pair s@(Pair a b) c -> iter (cons (deepReverse s) r) c
    Pair a b            -> iter (cons a r) b

-- Exercise 2.28, page 150
fringe l = case l of
  Nil      -> Nil
  Node _   -> l
  Pair a b -> append (fringe a) (fringe b)

-- Exercise 2.29, page 151
data Mobile = Mobile
  { leftBranch :: Branch
  , rightBranch :: Branch
  } deriving (Show, Eq)

data Branch = Branch
  { branchLength :: Int
  , branchStructure :: Structure
  } deriving (Show, Eq)

data Structure = Weight Int | Submobile Mobile deriving (Show, Eq)

-- part b
branchWeight b =
  case branchStructure b of
    Weight w -> w
    Submobile m -> mobileWeight m

mobileWeight m =
  branchWeight (leftBranch m) + branchWeight (rightBranch m)

-- part c
balancedBranch b =
  case branchStructure b of
    Weight _ -> True
    Submobile m -> balancedMobile m

branchTorque b = branchLength b * branchWeight b

balancedMobile m =
  branchTorque (leftBranch m) == branchTorque (rightBranch m) &&
  balancedBranch (leftBranch m) &&
  balancedBranch (rightBranch m)

-- part d
-- We used records, and it was easier.

-- testing
-- this mobile is balanced
mb = Mobile
       (Branch 3 (Weight 40))
       (Branch 10 (Submobile
         (Mobile
           (Branch 1 (Weight 8))
           (Branch 2 (Weight 4)))))

-- this mobile is not balanced
mu = Mobile
       (Branch 13 (Weight 10))
       (Branch 10 (Submobile
         (Mobile
           (Branch 1 (Weight 9))
           (Branch 2 (Weight 4)))))

-- Mapping over trees, page 152
scaleTree tree factor = case tree of
  Nil      -> Nil
  Node a   -> Node (factor * a)
  Pair a b -> cons (scaleTree a factor) (scaleTree b factor)

-- see above for fmap implementation
scaleTree' tree factor = fmap (* factor) tree

-- Exercise 2.30, page 153
square x = x * x

squareTree tree = case tree of
  Nil      -> Nil
  Node a   -> Node (square a)
  Pair a b -> cons (squareTree a) (squareTree b)

squareTree' tree = fmap square tree

-- Exercise 2.31, page 153
-- see Functor instance above
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap = fmap

-- this version of map operates on the first level list structure
map' :: (Tree a -> Tree b) -> Tree a -> Tree b
map' f t = case t of
  Nil      -> Nil
  Node _   -> f t
  Pair a b -> cons (f a) (map' f b)

-- Exercise 2.32, page 154
subsets s = case s of
  Nil      -> cons Nil Nil
  Node _   -> cons s Nil
  Pair a b -> let rest = subsets b
    in append rest (map' (cons a) rest)

-- This works by letting rest be the set of all subsets of s that do not contain
-- the first element of s, and appending this to the list containing each element
-- of rest but with the first element tacked on. In other words, for every subset
-- in rest, we have the original subset without the first element and another
-- subset with the first element. The concatenation of these two lists of subsets
-- will contain all possible subsets.
