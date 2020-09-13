import Data.Foldable (Foldable (null))
import Data.Sequence as Seq (Seq, fromList, index, update, (!?))
import System.IO (isEOF)

getOps :: Seq Int -> Int -> (Int, Int, Int)
getOps array i =
  (op1, op2, op3)
  where
    op1 = array `index` (array `index` (i + 1))
    op2 = array `index` (array `index` (i + 2))
    op3 = array `index` (i + 3)

performOp :: (Int -> Int -> Int) -> Seq Int -> Int -> Seq Int
performOp op array index =
  newArray
  where
    (op1, op2, op3) = getOps array index
    newValue = op op1 op2
    newArray = update op3 newValue array

handleCalcs :: Seq Int -> Int -> IO ()
handleCalcs array index =
  do
    putStrLn $ "Index: " ++ show index
    putStrLn $ show array

    case array !? index of
      Just 1 -> do
        putStrLn "Sum"
        handleCalcs (performOp (+) array index) $ index + 4
      Just 2 -> do
        putStrLn "Multiply"
        handleCalcs (performOp (*) array index) $ index + 4
      Just 99 ->
        putStrLn "Exit"
      Just _ ->
        putStrLn "Invalid opcode"
      Nothing ->
        putStrLn "No value here"

startCalc :: [Int] -> IO ()
startCalc array =
  handleCalcs s 0
  where
    s = fromList array

main = do
  iseof <- isEOF
  if iseof
    then putStrLn $ "No input!"
    else do
      line <- getLine
      if Data.Foldable.null line
        then putStrLn $ "No input"
        else do
          let array = read line :: [Int]
          startCalc array
