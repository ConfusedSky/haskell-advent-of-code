{-# LANGUAGE NamedFieldPuns #-}

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Show)

data Direction = U | R deriving (Eq, Show)

data LineSegment = LineSegment
  { origin :: Point,
    direction :: Direction,
    len :: Int
  }
  deriving (Show)

createSegment x y d l =
  LineSegment {origin = Point {x, y}, direction = d, len = l}

lineCollision :: LineSegment -> LineSegment -> Maybe Point
lineCollision first second =
  if parallel
    then Nothing
    else case direction first of
      U ->
        if ox first > ox second
          && ox first < ox second + len second
          && oy second > oy first
          && oy second < oy first + len first
          then Just $ Point (ox first) (oy second)
          else Nothing
      -- Handle only one of the two cases
      -- And flip the other
      R -> lineCollision second first
  where
    parallel = direction first == direction second
    ox = x . origin
    oy = y . origin