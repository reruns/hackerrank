import Control.Monad
import Data.List
import Data.Maybe


turan n m = find (\x -> m <= div ((n^2) * (x-1)) (2*x)) [2..]

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        str <- getLine
        let [n,m] = map read $ words str
        print $ fromJust $ turan n m
