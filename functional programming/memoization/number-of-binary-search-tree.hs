import Control.Monad

memoized_cat :: Int -> Integer
memoized_cat = (map catalan [0..] !!) where
  catalan 0 = 1
  catalan 1 = 1
  catalan x = div (memoized_cat (x-1) * (4 * n^2 - 2*n)) (n^2 + n) where
          n = toInteger x

ct n = mod (memoized_cat n) 100000007

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn
    print $ ct n
