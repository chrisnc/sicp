-- Nested Mappings, page 166

module Nested
  (
  ) where

import Prime
import Sequence
import Tree

flatmap proc seq = accumulate append Nil (fmap proc seq)

sumIsPrime pair = prime (fst pair + snd pair)

makePairSum pair = (fst pair, snd pair, fst pair + snd pair)

primeSumPairs n =
  fmap makePairSum $
    filter' sumIsPrime $
      flatmap
        (\i -> fmap (\j -> (i,j))
                    (enumerateInterval 1 (i - 1)))
        (enumerateInterval 1 n)

remove x = filter' (/= x)

permutations s = case s of
  Nil  -> list [Nil]
  _    -> flatmap (\x -> map' (x |:>) (permutations (remove x s))) s


-- Exercise 2.40, page 169
uniquePairs n =
  flatmap (\i -> fmap (\j -> (i,j))
                      (enumerateInterval 1 (i - 1)))
          (enumerateInterval 1 n)

primeSumPairs' = fmap makePairSum . filter' sumIsPrime . uniquePairs

-- Exercise 2.41, page 169
uniqueTriples n =
  flatmap (\k ->
            flatmap (\j ->
                      fmap (\i -> (i,j,k))
                           (enumerateInterval 1 (j - 1)))
                    (enumerateInterval 1 (k - 1)))
          (enumerateInterval 1 n)

tripleSumPairs n s = filter' (\(a,b,c) -> a + b + c == s) (uniqueTriples n)

-- Exercise 2.42, page 169
-- reverting to regular Haskell lists for this one, as it's much cleaner.
-- flatmap is just concatMap

adjoinPosition :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
adjoinPosition rest pos = rest ++ [pos]

queenCheck (r1,c1) (r2,c2) =
  r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)

isSafe :: [(Int,Int)] -> (Int,Int) -> Bool
isSafe positions qp = not $ any (queenCheck qp) positions

queens :: Int -> [[(Int,Int)]]
queens boardSize = queenCols boardSize where
  queenCols k =
    if k == 0
      then [[]]
      else concatMap
              (\restOfQueens -> fmap (adjoinPosition restOfQueens) $
                filter (isSafe restOfQueens) $
                  fmap (\r -> (r,k)) [1..boardSize])
              (queenCols (k - 1))

{-|
; Exercise 2.43, page 172
; for each potential new row, it attempts to re-arrange previous rows' queens
; such that the current queen's position is not in check, rather than choosing
; the current queen's position such that it is not in check given the previous
; queen's placements
; each call to queen-cols n generates board-size calls to queen-cols (n - 1)
; so the growth is exponential with a branching factor of board-size, and
; initially n = board-size, so this algorithm has complexity at least
; O((board-size)^(board-size))
; ignoring the work done in the map and flatmap
; TODO: make this a bit more rigorous
--}
