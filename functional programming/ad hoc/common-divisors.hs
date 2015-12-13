import Control.Monad

divs :: Int -> Int -> Int
divs l m = length $ [x | x <- [1..k], mod k x == 0]
           where k = gcd l m

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    str <- getLine
    let [l,m] = map read $ words str
    print $ divs l m
