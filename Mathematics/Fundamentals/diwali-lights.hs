import Control.Monad

solve n = mod (2^n - 1) 100000

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- readLn :: IO Int
        print $ solve n