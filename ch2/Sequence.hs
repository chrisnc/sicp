-- Section 2.2.3, Sequences as Conventional Interfaces, page 154

module Sequence
  ( sumOddSquares
  , fib
  , evenFibs
  , accumulate
  , accumulate'
  , enumerateInterval
  , enumerateTree
  , sumOddSquares'
  , evenFibs'
  , listFibSquares
  , productOfSquaresOfOddElements
  , map''
  , append'
  , length'
  , hornerEval
  , countLeaves'
  , accumulateN
  , zipWithTree
  , dotProduct
  , matrixByVector
  , transpose
  , matrixByMatrix
  , foldLeft'
  , foldRight
  , foldLeft
  , reverser
  , reversel
  ) where

import Tree
import qualified Data.Foldable as Foldable (foldr,foldl)
import Data.Function (on)

square x = x * x

sumOddSquares tree = case tree of
  Nil      -> 0
  Node a   -> if odd a then square a else 0
  Pair a b -> sumOddSquares a + sumOddSquares b

fib = iter 1 0 where
  iter a b n
    | n == 0 = b
    | otherwise  = iter (a + b) a (n - 1)

evenFibs n = next 0 where
  next k
    | k > n     = Nil
    | otherwise =
        let f = fib k in
          if even f
            then Node f <:> next (k + 1)
            else next (k + 1)

accumulate' op initial sequence = case sequence of
  Nil      -> initial
  Node _   -> op sequence initial
  Pair a b -> op a (accumulate' op initial b)

-- version of accumulate that relies on the underlying type a of a Tree a
-- using the Foldable instance of Tree
accumulate = Foldable.foldr

enumerateInterval low high =
  if low > high
    then Nil
    else Node low <:> enumerateInterval (low + 1) high

enumerateTree tree = case tree of
  Nil      -> Nil
  Node _   -> list [tree]
  Pair a b -> append (enumerateTree a) (enumerateTree b)

sumOddSquares' = accumulate (+) 0 . fmap square . filterTree odd

evenFibs' = accumulate (|:>) Nil . filterTree even . fmap fib . enumerateInterval 0

listFibSquares = accumulate (|:>) Nil . fmap square . fmap fib . enumerateInterval 0

productOfSquaresOfOddElements =
  accumulate (*) 1 .
  fmap square .
  filterTree odd

data Employee = Employee
  { salary :: Int
  , isProgrammer :: Bool
  }

salaryOfHighestPaidProgrammer =
  accumulate max 0 .
  fmap salary .
  filterTree isProgrammer

-- Exercise 2.33, page 161
-- essentially a flatmap
map'' p sequence = accumulate (\x y -> p x |:> y) Nil sequence

append' seq1 seq2 = accumulate (|:>) seq2 seq1

length' :: Tree a -> Int
length' = accumulate (\x y -> y + 1) 0

-- Exercise 2.34, page 162
hornerEval x coeffs =
  accumulate (\tc ht -> tc + (x * ht)) 0 coeffs

-- Exercise 2.35, page 163
countLeaves' :: Tree a -> Int
countLeaves' = accumulate (+) 0 . fmap (const 1)

-- Exercise 2.36, page 163
accumulateN :: (a -> b -> b) -> b -> Tree a -> Tree b
accumulateN op init seqs = case seqs of
  Nil        -> Nil
  Node _     -> Nil
  Pair Nil _ -> Nil
  Pair a b   -> accumulate op init (map' car seqs) |:>
                accumulateN op init (map' cdr seqs)

-- combine two trees with the same structure using op
-- if a subtree does not have the same corresponding structure
-- in the other tree, the result is Nil
zipWithTree op t u = case (t,u) of
  (Node a,   Node b)   -> Node (op a b)
  (Pair a b, Pair c d) -> zipWithTree op a c <:> zipWithTree op b d
  _                    -> Nil

-- Exercise 2.37, page 163
dotProduct :: Num a => Tree a -> Tree a -> a
dotProduct v w = accumulate (+) 0 (zipWithTree (*) v w)

matrixByVector :: Num a => Tree a -> Tree a -> Tree a
matrixByVector m v = map' (Node . dotProduct v) m

unTree :: Tree (Tree a) -> Tree a
unTree x = case x of
  Nil      -> Nil
  Node a   -> a
  Pair a b -> Pair (unTree a) (unTree b)

transpose :: Tree a -> Tree a
transpose = unTree . accumulateN (|:>) Nil

matrixByMatrix m n = map' (matrixByVector (transpose n)) m

-- Exercise 2.38, page 165
foldLeft' op initial sequence = iter initial sequence where
  iter result rest = case rest of
    Nil -> result
    _   -> iter (op result (car rest)) (cdr rest)

foldRight = Foldable.foldr
foldLeft = Foldable.foldl

showFolds = do
 print $ foldRight (/) 1 (list' [1,2,3])
 -- 3/2
 print $ foldLeft (/) 1 (list' [1,2,3])
 -- 1/6
 print $ foldRight (\x y -> list [Node x,y]) Nil (list' [1,2,3])
 -- '(1 (2 (3 ())))
 print $ foldLeft (\x y -> list [x,Node y]) Nil (list' [1,2,3])
 -- '(((() 1) 2) 3)

-- fold-right and fold-left produce the same value for any sequence only when op
-- is associative a fold is just interspersing elements of the sequence with op
-- where the fold decides how the operands will associate with an associative
-- operator, it does not matter how the operands are associated, by definition

-- Exercise 2.39, page 166
reverser = foldRight (\x l -> append l (list' [x])) Nil

reversel = foldLeft (\l x -> cons (Node x) l) Nil
