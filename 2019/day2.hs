import Data.List
import System.Environment
import System.IO

getOps array index =
  (op1, op2, op3)
  where
    op1 = array !! (array !! (index + 1))
    op2 = array !! (array !! (index + 2))
    op3 = array !! (index + 3)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n -1) newVal xs

performOp op array index =
  newArray
  where
    (op1, op2, op3) = getOps array index
    newValue = op op1 op2
    newArray = replaceNth op3 newValue array

handleCalcs array index =
  do
    putStrLn $ "Index: " ++ show index
    putStrLn $ show array

    case array !! index of
      1 -> do
        putStrLn "Sum"
        handleCalcs (performOp (+) array index) $ index + 4
      2 -> do
        putStrLn "Multiply"
        handleCalcs (performOp (*) array index) $ index + 4
      99 ->
        putStrLn "Exit"
      _ ->
        putStrLn "Invalid opcode"

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
          handleCalcs array 0
