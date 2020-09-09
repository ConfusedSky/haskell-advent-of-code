import Data.List
import System.Environment
import System.IO

doCalc :: Int -> Int
doCalc x =
  floor (fromIntegral (x) / 3) - 2

recurseCalc :: Int -> Int -> Int
recurseCalc total x
  | value <= 0 = total
  | otherwise = recurseCalc (total + value) value
  where
    value = doCalc x

mainHelper total =
  do
    iseof <- isEOF
    if iseof
      then putStrLn $ "Total: " ++ (show total)
      else do
        line <- getLine
        if null line
          then putStrLn $ "Total: " ++ (show total)
          else do
            let fuel = recurseCalc 0 (read line)
            putStrLn (show fuel)
            mainHelper $ total + fuel

main = do
  mainHelper 0