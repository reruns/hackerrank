import Control.Monad

mknap :: [Int] -> Int -> Int
mknap a k = (map (knap a) [1..k] !!) (k-1)

--When all the coins are smaller than k, we get 0
knap :: [Int] -> Int -> Int
knap a k = f a k 0 where
 f [] _ n = n
 f (b:bs) k n | b > k = f bs k n
              | b == k = k
              | k' + b == k = k
              | k' + b < k = f bs k n
              | otherwise = f bs k (max n $ k' + b)
              where k' = mknap (filter (<k) a) (k-b)

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        str1 <- getLine
        str2 <- getLine
        let n = last $ map read $ words str1
            a = map read $ words str2
        print $ mknap a n
