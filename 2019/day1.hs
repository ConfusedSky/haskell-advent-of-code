import Data.List
import System.Environment

doCalc x =
  floor $ x / 3 - 2

main = do
  args <- getArgs
  content <- getContents
  let fuels = (map (doCalc . read) $ lines content)
  putStrLn (show (sum fuels))