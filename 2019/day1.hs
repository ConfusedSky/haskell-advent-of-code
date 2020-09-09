import Data.List
import System.Environment
import System.IO

doCalc :: Int -> Int
doCalc x =
  floor (fromIntegral (x) / 3) - 2

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
            let fuel = doCalc (read line)
            putStrLn (show fuel)
            mainHelper $ total + fuel

main = do
  mainHelper 0