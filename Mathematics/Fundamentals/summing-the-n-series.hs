import Control.Monad

--Very funny.
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- readLn :: IO Integer
        print $ mod (n*n) (10^9 + 7)