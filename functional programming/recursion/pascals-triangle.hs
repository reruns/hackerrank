pascals 1 = [[1]]
pascals 2 = [1,1] : [[1]]
pascals n = ps : (pascals (n-1))
            where xs = head $ pascals (n-1)
                  ps = 1 : (zipWith (+) xs (tail xs)) ++ [1]

main = do
  k <- readLn :: IO Int
  let ans = pascals k
  print ans
