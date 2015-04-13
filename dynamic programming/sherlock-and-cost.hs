-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

--this assumes a naive solution that is not correct :<
evens (x:[]) _ = 0
evens (x:xs) c = k + evens xs (not c) where
    k = if c then (head xs) - 1 else x-1

solve bs = max (evens bs False) (evens bs True)

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- getLine
        bstr <- getLine
        let bs = map read $ words bstr
        print $ solve bs
