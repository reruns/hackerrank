l x = map fst $ scanl f (1,head x) (tail x) where
  f (a,b) y = if y > b then (a+1,y) else (1,y)

solve lst = foldl1 (+) $ zipWith max (l lst) (reverse $ l $ reverse lst)

main = do
  t <- readLn :: IO Int
  lst <- replicateM t (readLn :: IO Int)
  print $ solve lst
