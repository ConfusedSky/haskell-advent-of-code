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
    len :: Int
  }

instance Show LineSegment where
  show (LineSegment o d l) =
    show o
      ++ " "
      ++ show d
      ++ " "
      ++ show l
      ++ "\n"

createSegment x y d l =
  LineSegment (Point x y) d l

shiftPoint (Point {x, y}) dir amount =
  case dir of
    U ->
      Point x (y + amount)
    R ->
      Point (x + amount) y

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

lineCollisions :: [LineSegment] -> LineSegment -> [Point]
lineCollisions l second =
  catMaybes
    . map (lineCollision second)
    $ l

linesCollisions :: [LineSegment] -> [LineSegment] -> [Point]
linesCollisions l l2 =
  concat
    . map (lineCollisions l2)
    $ l

answer l1 l2 =
  minimum
    . map (\(Point x y) -> (abs x) + (abs y))
    $ linesCollisions l1 l2

parseLine p (d : value) =
  case d of
    'R' ->
      ( LineSegment p R distance,
        shiftPoint p R distance
      )
    'L' ->
      ( LineSegment (shiftPoint p R (- distance)) R distance,
        shiftPoint p R (- distance)
      )
    'U' ->
      ( LineSegment p U distance,
        shiftPoint p U distance
      )
    'D' ->
      ( LineSegment (shiftPoint p U (- distance)) U distance,
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

parseString =
  parseLines
    . splitOn ","

-- Use this function in ghci to get the correct answer
fromStrings s s2 =
  answer (parseString s) (parseString s2)