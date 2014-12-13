-- Section 2.3, Symbolic Data, page 192

module Symbolic
  (
  ) where

import Tree

memq item x =
  case x of
    Pair a _ | a == item -> x
    Pair _ b             -> memq item b
    _                    -> Nil

-- Exercise 2.53, page 195

isPair x =
  case x of
    Pair _ _ -> True
    _        -> False

main :: IO ()
main = do
  print $ list' ["a", "b", "c"]
  -- (a b c)
  print $ list [list' ["george"]]
  -- ((george))
  print $ cdr $ list [list' ["x1","x2"], list' ["y1", "y2"]]
  -- ((y1 y2))
  print $ car . cdr $ list [list' ["x1","x2"], list' ["y1", "y2"]]
  -- '(y1 y2)
  print $ isPair $ car $ list' ["a", "short", "list"]
  -- #f
  print $ memq (Node "red") $ list [list' ["red", "shoes"], list' ["blue", "socks"]]
  -- ()
  print $ memq (Node "red") $ list' ["red", "shoes", "blue", "socks"]
  -- '(red shoes blue socks)

-- Exercise 2.54, page 196
equal :: Eq a => Tree a -> Tree a -> Bool
equal = (==)

-- our Tree type derives Eq

{-|
; Exercise 2.55, page 196
(car ''abracadabra)
; this expands to
(car (quote 'abracadabra))
; which expands to
(car (quote (quote abracadabra)))
; rewritten again
(car '(quote abracadabra))
; now it's obvious that the car of '(quote abracadabra) is 'quote
--}
