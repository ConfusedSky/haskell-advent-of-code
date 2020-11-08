printRange first last =
  sequence . map print $ [first .. last]

walkDigits :: Integral x => [x] -> x -> Bool -> Bool
walkDigits [] _ hasRepeat = hasRepeat
walkDigits (digit : rest) highest hasRepeat =
  if decreasing
    then False
    else walkDigits rest digit newRepeat
  where
    newRepeat = digit == highest || hasRepeat
    decreasing = digit < highest

checkCorrectness value =
  ((== 6) . length $ digits)
    && (walkDigits digits 0 False)
  where
    digits = digs value

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

countRange first last =
  length
    . filter checkCorrectness
    $ [first .. last]

main = print (countRange 136818 685979)