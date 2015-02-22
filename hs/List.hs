-- Section 2.2.1, Representing Sequences, page 134

module List
  ( lref
  , length'
  , length''
  , append
  , lastPair
  , reverse'
  , usCoins
  , ukCoins
  , cc
  ) where

lref items n =
  if n == 0
    then head items
    else lref (tail items) (n - 1)

length' items =
  if null items
    then 0
    else 1 + length' (tail items)

length'' items = iter items 0 where
  iter a count =
    if null a
      then count
      else iter (tail a) (1 + count)

append a b =
  if null a
    then b
    else head a : append (tail a) b

-- Exercise 2.17, page 139
lastPair l
  | null l       = []
  | null (tail l) = head l
  | otherwise    = lastPair (tail l)

-- Exercise 2.18, page 140
reverse' l = iter [] l where
  iter r l =
    if null l
      then r
      else iter (head l : r) (tail l)

-- Exercise 2.19, page 140
usCoins = [50, 25, 10, 5, 1]
ukCoins = [100, 50, 20, 10, 5, 2, 1, 0.5]

firstDenomination = head
exceptFirstDenomination = tail
noMore = null

cc amount coins
  | amount == 0              = 1
  | amount < 0 || null coins = 0
  | otherwise                =
      cc amount (exceptFirstDenomination coins) +
      cc (amount - firstDenomination coins) coins

-- The order of the coins does not matter, since the two paths of the recursion
-- capture all possible ways to use the coins to produce amount.
-- Either a combination does not use any of a particular coin, or it uses
-- one or more of that coin, and some number of the other coins, in either case.
-- No possibility is excluded by encountering a particular denomination first.

-- Exercise 2.20, page 141
sameParity x l = x : parityRest l where
  parityRest l
    | null l                    = []
    | rem x 2 == rem (head l) 2 = head l : parityRest (tail l)
    | otherwise                 = parityRest (tail l)

-- Mapping over lists, page 143
scaleList items factor =
  if null items
    then []
    else head items * factor : scaleList (tail items) factor

map' proc items =
  if null items
    then []
    else proc (head items) : map proc (tail items)

scaleList' items factor = map (* factor) items

-- Exercise 2.21, page 144
square x = x * x

squareList items =
  if null items
    then []
    else square (head items) : squareList (tail items)

squareList' items = map square items

-- Exercise 2.22, page 145

squareList'' items = iter items [] where
  iter things answer =
    if null things
      then answer
      else iter (tail things) (square (head things) : answer)

-- the list answer is constructed by consing the head of things with answer
-- for each element in items, so the items are appended to the head of answer
-- in the order they are encountered in items

{-|
squareList''' items = iter items [] where
  iter things answer =
    if null things
      then answer
      else iter (tail things) (answer : square (head things))

-- In Haskell, this is a type error, as we are cons'ing a list onto a non-list,
-- which is backwards. In Scheme, this will run, but the result is not a list.
--}

-- we can fix it by just calling reverse on the result of the first
-- implementation
squareList''' items = reverse (iter items []) where
  iter things answer =
    if null things
      then answer
      else iter (tail things) (square (head things) : answer)

-- Exercise 2.23, page 146
forEach f l =
  if null l
    then return ()
    else f (head l) >> forEach f (tail l)
