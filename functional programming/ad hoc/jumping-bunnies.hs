bunny xs = foldl lcm 1 xs

main = do
  t <- getLine
  str <- getLine
  let
    nums = map read $ words str
    ans = bunny nums
  print ans
