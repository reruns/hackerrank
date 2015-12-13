import Control.Monad
import Data.List

solve xs = let s = sum xs in
           if (sol 0 s xs) then "YES" else "NO"

sol _ _ [] = False
sol l r (x:xs) = if l == (r-x) then True
                 else sol (l+x) (r-x) xs

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn :: IO Int
    str <- getLine
    putStrLn $ solve $ map (\x -> read x :: Int) (words str)
