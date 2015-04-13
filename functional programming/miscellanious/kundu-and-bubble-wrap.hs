bubs n = foldl1 (+) $ map (n/) [1..n]

main = do
  str <- getLine
  let [a,b] = map read $ words str
  print $ bubs (a*b)
