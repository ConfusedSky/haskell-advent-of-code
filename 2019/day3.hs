{-# LANGUAGE NamedFieldPuns #-}

data Point = Point
  { x :: Int,
    y :: Int
  }

instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

center = Point 0 0

data Direction = U | R deriving (Eq, Show)

data LineSegment = LineSegment
  { origin :: Point,
    direction :: Direction,
    len :: Int
  }

instance Show LineSegment where
  show (LineSegment o d l) = show o ++ show d ++ show l ++ "\n"

createSegment x y d l =
  LineSegment {origin = Point {x, y}, direction = d, len = l}

shiftPoint (Point {x, y}) dir amount =
  case dir of
    U ->
      Point {x = x, y = y + amount}
    R ->
      Point {x = x + amount, y = y}

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

parseLine p (d : value) =
  case d of
    'R' ->
      ( LineSegment {origin = p, direction = R, len = distance},
        shiftPoint p R distance
      )
    'L' ->
      ( LineSegment
          { origin = (shiftPoint p R (- distance)),
            direction = R,
            len = distance
          },
        shiftPoint p R (- distance)
      )
    'U' ->
      ( LineSegment {origin = p, direction = U, len = distance},
        shiftPoint p U distance
      )
    'D' ->
      ( LineSegment
          { origin = (shiftPoint p U (- distance)),
            direction = U,
            len = distance
          },
        shiftPoint p U (- distance)
      )
  where
    distance = read value :: Int

parseLinesHelper [] a _ = a
parseLinesHelper (x : xs) a p =
  parseLinesHelper xs (line : a) newP
  where
    (line, newP) = parseLine p x

parseLines d =
  reverse $
    parseLinesHelper d [] center