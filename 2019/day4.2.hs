walkDigits :: Integral x => [x] -> x -> Bool -> Int -> Bool
walkDigits [] _ hasRepeat repeatCount = hasRepeat || repeatCount == 2
walkDigits (digit : rest) highest hasRepeat repeatCount =
  if decreasing
    then False
    else walkDigits rest digit newRepeat newRepeatCount
  where
    newRepeat = hasRepeat || (not digitRepeat && repeatCount == 2)
    digitRepeat = digit == highest
    newRepeatCount = if digitRepeat then repeatCount + 1 else 1
    decreasing = digit < highest

checkCorrectness value =
  ((== 6) . length $ digits)
    && (walkDigits digits 0 False 1)
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