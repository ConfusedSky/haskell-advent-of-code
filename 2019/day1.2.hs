import Data.List
import System.Environment
import System.IO

doCalc :: Int -> Int
doCalc x =
  floor (fromIntegral (x) / 3) - 2

recurseCalc :: [Int] -> Int -> [Int]
recurseCalc total x
  | value <= 0 = reverse total
  | otherwise = recurseCalc (value : total) value
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
            let fuel = recurseCalc [] (read line)
            let totalFuel = sum fuel
            putStrLn $ "Steps: " ++ show fuel
            putStrLn $ "Fuel: " ++ show totalFuel
            mainHelper $ total + totalFuel

main = do
  mainHelper 0