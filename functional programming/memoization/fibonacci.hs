import Control.Monad

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn :: IO Int
    print $ mod (memoized_fib n) 100000007
