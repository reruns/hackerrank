-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad
import Data.List
import Data.Maybe

solve x1 y1 x2 y2 | (x1==x2) = (abs $ (y2-y1))-1
                  | otherwise = check start
    where start = min (x1+1) (x2+1)
          end = max x1 x2
          check n | n == end = 0
                  | 0==mod (n*(y2-y1)+y1*x2-y2*x1) (x2-x1) = div (end-start) (n-start+1)
                  | otherwise = check (n+1)

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        str <- getLine
        let [x1,y1,x2,y2] = map (\x -> read x :: Int) $ words str
        print $ solve x1 y1 x2 y2
