import Control.Monad

--Not fast enough :<
--modular exponentiation
solve :: Integer -> Integer -> Integer
solve x y = f x y 1 where
  m = 10^9+7
  f b 0 r = r
  f b e r | odd e = f (b*b `mod` m) (e `div` 2) (r*b `mod` m)
          | otherwise = f (b*b `mod` m) (e `div` 2) r

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    cstr <- getLine
    let [a,b] = map read $ words cstr
    print $ solve a b
