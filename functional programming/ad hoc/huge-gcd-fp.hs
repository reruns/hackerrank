main = do
  (n,ast,m,bst) <- (getLine, getLine, getLine, getLine)
  let
    a = foldl1 (*) $ map read $ words ast
    b = foldl1 (*) $ map read $ words bst
  print $ mod (gcd a b) 1000000007
