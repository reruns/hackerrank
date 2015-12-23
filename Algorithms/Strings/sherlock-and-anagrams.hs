import Data.List
import Control.Monad

substrings xs = concat $ scanl (\r x -> [x]:(map (x:) r)) [] xs 

solve x = (foldr (\x r -> r + div ((x-1)*x) 2) 0 $ (map length) ((group.sort) (map sort (substrings x))))

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        s <- getLine
        print $ solve s