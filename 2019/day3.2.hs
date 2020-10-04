{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

data Point = Point
  { x :: Int,
    y :: Int
  }

instance Show Point where
  show (Point x y) =
    "("
      ++ show x
      ++ ", "
      ++ show y
      ++ ")"

center = Point 0 0

data Direction = U | R deriving (Eq, Show)

data LineSegment = LineSegment
  { origin :: Point,
    direction :: Direction,
    len :: Int,
    totalDistance :: Int,
    distanceFlip :: Bool
  }

instance Show LineSegment where
  show (LineSegment o d l t df) =
    show o
      ++ " "
      ++ show d
      ++ " "
      ++ show l
      ++ " "
      ++ show t
      ++ " "
      ++ show df
      ++ "\n"

createSegment x y d l =
  LineSegment (Point x y) d l

shiftPoint (Point {x, y}) dir amount =
  case dir of
    U ->
      Point x (y + amount)
    R ->
      Point (x + amount) y

lineCollision :: LineSegment -> LineSegment -> Maybe (Point, Int)
lineCollision first second =
  if parallel
    then Nothing
    else case direction first of
      U ->
        if ox first > ox second
          && ox first < ox second + len second
          && oy second > oy first
          && oy second < oy first + len first
          then (Just $ (Point (ox first) (oy second), td))
          else Nothing
      -- Handle only one of the two cases
      -- And flip the other
      R -> lineCollision second first
  where
    parallel = direction first == direction second
    ox = x . origin
    oy = y . origin
    td1 =
      totalDistance first
        + if distanceFlip first
          then oy first + len first - oy second
          else oy second - oy first

    td2 =
      totalDistance second
        + if distanceFlip second
          then ox second + len second - ox first
          else ox first - ox second
    td = td1 + td2

lineCollisions :: [LineSegment] -> LineSegment -> [(Point, Int)]
lineCollisions l second =
  catMaybes
    . map (lineCollision $ second)
    $ l

linesCollisions :: [LineSegment] -> [LineSegment] -> [(Point, Int)]
linesCollisions l l2 =
  concat
    . map (lineCollisions l2)
    $ l

answer l1 l2 =
  -- Take the first answer since that is always going to be the
  -- shortest one
  head
    . map (\(_, z) -> z)
    $ linesCollisions l1 l2

parseLine p (d : value) totald =
  case d of
    'R' ->
      ( LineSegment p R distance totald False,
        shiftPoint p R distance,
        totalDist
      )
    'L' ->
      ( LineSegment (shiftPoint p R (- distance)) R distance totald True,
        shiftPoint p R (- distance),
        totalDist
      )
    'U' ->
      ( LineSegment p U distance totald False,
        shiftPoint p U distance,
        totalDist
      )
    'D' ->
      ( LineSegment (shiftPoint p U (- distance)) U distance totald True,
        shiftPoint p U (- distance),
        totalDist
      )
  where
    distance = read value :: Int
    totalDist = totald + distance

parseLinesHelper [] a _ _ = a
parseLinesHelper (x : xs) a p td =
  parseLinesHelper xs (line : a) newP newTd
  where
    (line, newP, newTd) = parseLine p x td

parseLines d =
  reverse $
    parseLinesHelper d [] center 0

parseString =
  parseLines
    . splitOn ","

-- Use this function in ghci to get the correct answer
fromStrings s s2 =
  answer (parseString s) (parseString s2)
