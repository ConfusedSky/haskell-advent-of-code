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

handleCalcs :: Seq Int -> Int -> Int
handleCalcs array i =
  case array !? i of
    Just 1 -> do
      handleCalcs (performOp (+) array i) $ i + 4
    Just 2 -> do
      handleCalcs (performOp (*) array i) $ i + 4
    Just 99 ->
      array `index` 0
    _ ->
      -1

possibleInputs =
  concat $ map (\x -> map (\y -> (x, y)) l) l
  where
    l = [0 .. 99]

nounVerbResult array noun verb =
  handleCalcs newArray 0
  where
    nounChange = update 1 noun array
    newArray = update 2 verb nounChange

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
          let array = fromList $ (read line :: [Int])
          putStrLn . show $ getAnswer array 19690720
