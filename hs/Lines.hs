-- Exercise 2.2, page 121

module Lines
  ( Point(..)
  , Segment(..)
  , midPointSegment
  , printPoint
  , printSegment
  , RectWHC(..)
  , Rect
  , RectPoints(..)
  , rectPointsCenter
  , pointDistance
  ) where

data Point = Point
  { pointX :: Double
  , pointY :: Double
  }

instance Show Point where
  show p = "(" ++ show (pointX p) ++ "," ++ show (pointY p) ++ ")"

data Segment = Segment
  { startSegment :: Point
  , endSegment :: Point
  }

instance Show Segment where
  show s = "(" ++ show (startSegment s) ++ "," ++ show (endSegment s) ++ ")"

midPointSegment s =
  Point ((pointX a + pointX b) / 2) ((pointY a + pointY b) / 2) where
    a = startSegment s
    b = endSegment s

printPoint :: Point -> IO ()
printPoint = putStr . show

printSegment :: Segment -> IO ()
printSegment = putStr . show

a = Point 0 3
b = Point 4 9
c = Point 4 9
d = Point 4 9
l = Segment a b

-- Exercise 2.3, page 122

data RectWHC = RectWHC
  { _width :: Double
  , _height :: Double
  , _center :: Point
  }

-- interface for rectangles, required to implement width and height
class Rect a where
  width :: a -> Double
  height :: a -> Double
  area :: a -> Double
  perimeter :: a -> Double
  area r = width r * height r
  perimeter r = 2 * (width r + height r)

instance Rect RectWHC where
  width = _width
  height = _height

r1 = RectWHC 4 5 (Point 2 2.5)

-- second representation
-- assume the points a b c and d actually form a rectangle
-- with a in the top left and going clockwise to d

data RectPoints = RectPoints
  { rectA :: Point
  , rectB :: Point
  , rectC :: Point
  , rectD :: Point
  }

-- get the center by averaging the four points
rectPointsCenter r =
  midPointSegment $ Segment
    (midPointSegment $ Segment (rectA r) (rectB r))
    (midPointSegment $ Segment (rectC r) (rectD r))

square x = x * x

pointDistance a b =
  sqrt $ square (pointX a - pointX b) + square (pointY a - pointY b)

-- define width and height for this representation
instance Rect RectPoints where
  width r = pointDistance (rectA r) (rectB r)
  height r = pointDistance (rectA r) (rectD r)

-- define the same rectangle as r1, but using the new representation
r2 = RectPoints
  (Point 0 5)
  (Point 4 5)
  (Point 4 0)
  (Point 0 0)
