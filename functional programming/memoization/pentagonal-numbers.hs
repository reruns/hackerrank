import Control.Monad

--Why the hell would we memoize something with a simple closed form solution?
p n = div (3 * n^2 - n) 2

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn :: IO Int
    print $ p n
