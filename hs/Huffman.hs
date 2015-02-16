-- Example 2.3.4: Huffman Encoding Trees, page 218

module Huffman
  ( HuffmanTree
  , Bit(..)
  , decode
  , encode
  , generateHuffmanTree
  , sampleTree
  , sampleMessage
  , fiftiesRockTree
  , fiftiesRockMessage
  ) where

data HuffmanTree sym wt =
    Leaf sym wt
  | Tree [sym] wt (HuffmanTree sym wt) (HuffmanTree sym wt)
  deriving (Eq, Show)

data Bit = Zero | One
  deriving (Eq, Ord)

instance Show Bit where
  show Zero = "0"
  show One = "1"

makeCodeTree :: Num wt => HuffmanTree sym wt -> HuffmanTree sym wt -> HuffmanTree sym wt
makeCodeTree left right =
  Tree
    (symbols left ++ symbols right)
    (weight left + weight right)
    left
    right

symbols :: HuffmanTree sym wt -> [sym]
symbols tree =
  case tree of
    Leaf sym _ -> [sym]
    Tree syms _ _ _ -> syms

weight :: HuffmanTree sym wt -> wt
weight tree =
  case tree of
    Leaf _ wt -> wt
    Tree _ wt _ _ -> wt

decode :: HuffmanTree sym wt -> [Bit] -> [sym]
decode tree = decode1 tree where
  decode1 currentBranch bits =
    case (currentBranch, bits) of
      (Leaf sym _, bs)        -> sym : decode1 tree bs
      (Tree _ _ l _, Zero:bs) -> decode1 l bs
      (Tree _ _ _ r, One:bs)  -> decode1 r bs
      (_, [])                 -> []

adjoinSet :: Ord wt => HuffmanTree sym wt -> [HuffmanTree sym wt] -> [HuffmanTree sym wt]
adjoinSet x s =
  case s of
    [] -> [x]
    e:_ | weight x < weight e -> x : s
    e:es                      -> e : adjoinSet x es

makeLeafSet :: Ord wt => [(sym,wt)] -> [HuffmanTree sym wt]
makeLeafSet = foldr (adjoinSet . uncurry Leaf) []

-- Exercise 2.67, page 226
sampleTree :: Num wt => HuffmanTree Char wt
sampleTree =
  makeCodeTree
    (Leaf 'A' 4)
    (makeCodeTree
      (Leaf 'B' 2)
      (makeCodeTree
        (Leaf 'D' 1)
        (Leaf 'C' 1)))

toBitList :: [Char] -> [Bit]
toBitList = foldr
  (\c l -> case c of
    '0' -> Zero : l
    '1' -> One : l
    _   -> l) []

sampleMessage :: [Bit]
sampleMessage = toBitList "0 1 1 0 0 1 0 1 0 1 1 1 0"

-- Exercise 2.68, page 226

encode :: Eq sym => HuffmanTree sym wt -> [sym] -> [Bit]
encode = concatMap . encodeSymbol

encodeSymbol :: Eq sym => HuffmanTree sym wt -> sym -> [Bit]
encodeSymbol tree sym =
  case tree of
    Leaf _ _ -> []
    Tree _ _ l r | elem sym (symbols l) -> Zero : encodeSymbol l sym
                 | elem sym (symbols r) -> One  : encodeSymbol r sym
    _ -> error "this symbol can't be encoded"


-- Exercise 2.69, page 227
generateHuffmanTree :: (Num wt, Ord wt) => [(sym, wt)] -> HuffmanTree sym wt
generateHuffmanTree = successiveMerge . makeLeafSet

successiveMerge :: (Num wt, Ord wt) => [HuffmanTree sym wt] -> HuffmanTree sym wt
successiveMerge s =
  case s of
    [] -> error "no trees to merge"
    [tree] -> tree
    a:b:trees -> successiveMerge (adjoinSet (makeCodeTree b a) trees)

-- Exercise 2.70, page 228
fiftiesRockTree :: (Num wt, Ord wt) => HuffmanTree String wt
fiftiesRockTree =
  generateHuffmanTree
    [("A",2),
     ("GET",2),
     ("SHA",3),
     ("WAH",1),
     ("BOOM",1),
     ("JOB",2),
     ("NA",16),
     ("YIP",9)]

fiftiesRockMessage :: [String]
fiftiesRockMessage =
  ["GET", "A", "JOB", "SHA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
  "GET", "A", "JOB", "SHA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
  "WAH", "YIP", "YIP", "YIP", "YIP", "YIP", "YIP", "YIP", "YIP", "YIP", "SHA",
  "BOOM"]

{--
 -- See rkt/huffman.rkt for answers to exercise 2.71 and 2.72, which don't
 -- involve programming.
 --}
