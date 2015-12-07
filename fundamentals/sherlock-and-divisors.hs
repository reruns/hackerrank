import Control.Monad

ans x | odd x = 0
      | otherwise = countDivisors (div x 2)

countDivisors x = let n = length [ z | z <- [1..(floor.sqrtI) x], mod x z == 0] in
                  if (floor.sqrtI) x == (ceiling.sqrtI) x
                  then 2 * n - 1
                  else 2 * n
                  
sqrtI = sqrt.fromIntegral

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        x <- readLn :: IO Int
        print $ ans x