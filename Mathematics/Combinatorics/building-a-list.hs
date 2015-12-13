subsets [] = []
subsets (x:xs) = map (x:) $ subsets xs

main = do
  print $ subsets "abc"
