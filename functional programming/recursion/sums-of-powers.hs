sop x n = length $ filter (x==) (map (foldl (+) 0) (subsets z x))
          where z = [p^n | p <- [1..x] , (p^n) <= x]

subsets [] n = [[]]
subsets (x:xs) n = sub ++ map (x:) sub where
               sub = filter (\y -> n >= (foldl (+) 0 y)) $ subsets xs n

main = do
  [x,n] <- sequence [readLn :: IO Integer, readLn :: IO Integer]
  print $ sop x n
