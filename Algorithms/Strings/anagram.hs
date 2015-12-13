import Data.List
import Control.Monad

solve text | odd (length text) = -1
           | otherwise = length (q \\ p) where
           (q,p) = splitAt (div (length text) 2) text

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    text <- getLine
    print $ solve text
