import Data.List
import Control.Monad

isSorted [x] = True
isSorted (x:xs) = if x > (head xs) then False else isSorted xs

solve grid = if all isSorted (transpose $ map sort grid) then "YES" else "NO"


main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn :: IO Int
    grid <- replicateM n getLine
    putStrLn $ solve grid
