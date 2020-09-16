{-# LANGUAGE NamedFieldPuns #-}

data Point = Point
  { x :: Int,
    y :: Int
  }

data Direction = U | R deriving (Eq)

data LineSegment = LineSegment
  { origin :: Point,
    direction :: Direction,
    len :: Int
  }

createSegment x y d l =
  LineSegment {origin = Point {x, y}, direction = d, len = l}

lineCollision :: LineSegment -> LineSegment -> Bool
lineCollision first second =
  not parallel
    && case direction first of
      U ->
        ox first > ox second
          && ox first < ox second + len second
          && oy second > oy first
          && oy second < oy first + len first
      -- Handle only one of the two cases
      -- And flip the other
      R -> lineCollision second first
  where
    parallel = direction first == direction second
    ox = x . origin
    oy = y . origin