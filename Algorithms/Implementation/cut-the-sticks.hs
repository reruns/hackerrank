import Data.List
import Control.Monad

solve xs = sol (group $ sort xs) (length xs)
sol _ 0 = []
sol (x:xs) y = y:(sol xs (y - length x))

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- getLine
  let
    nums = map (\x -> read x :: Int) (words str)
    ans = solve nums
  mapM_ print ans
