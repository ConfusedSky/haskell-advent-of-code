import Data.List
import System.Environment

doCalc :: Int -> Int
doCalc x =
  floor (fromIntegral (x) / 3) - 2

recurseCalc :: Int -> Int -> Int
recurseCalc total x
  | value <= 0 = total
  | otherwise = recurseCalc (total + value) value
  where
    value = doCalc x

main = do
  args <- getArgs
  content <- getContents
  let fuels = (map ((recurseCalc 0) . read) $ lines content)
  putStrLn (show (sum fuels))