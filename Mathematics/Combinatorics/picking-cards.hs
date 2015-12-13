cards cs k = if k > length cards then 1 else
             (cards cs (k+1)) * ((length $ filter (<=k) cs)-k)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn :: IO Int
    str <- getLine
    let nums = map read $ words str
    print $ cards nums 0
