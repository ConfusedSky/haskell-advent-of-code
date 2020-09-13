import Data.List
import System.Environment
import System.IO

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

getOps array index =
  (op1, op2, op3)
  where
    op1 = array !! (array !! (index + 1))
    op2 = array !! (array !! (index + 2))
    op3 = array !! (index + 3)

performOp op array index =
  newArray
  where
    (op1, op2, op3) = getOps array index
    newValue = op op1 op2
    newArray = replaceNth op3 newValue array

handleCalcs array index =
  case array !! index of
    1 -> do
      handleCalcs (performOp (+) array index) $ index + 4
    2 -> do
      handleCalcs (performOp (*) array index) $ index + 4
    99 ->
      array !! 0
    _ ->
      -1

possibleInputs =
  concat $ map (\x -> map (\y -> (x, y)) l) l
  where
    l = [0 .. 99]

nounVerbResult array noun verb =
  handleCalcs newArray 0
  where
    nounChange = replaceNth 1 noun array
    newArray = replaceNth 2 verb nounChange

getAnswer array value =
  head
    . map (\(noun, verb, _) -> noun * 100 + verb)
    . filter (\(_, _, x) -> x == value)
    . map (\(x, y) -> (x, y, nounVerbResult array x y))
    $ possibleInputs

main = do
  iseof <- isEOF
  if iseof
    then putStrLn $ "No input!"
    else do
      line <- getLine
      if null line
        then putStrLn $ "No input"
        else do
          let array = read line :: [Int]
          putStrLn . show $ getAnswer array 19690720
