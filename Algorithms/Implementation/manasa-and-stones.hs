import Control.Monad
import Data.List

solve [] = []
solve nums = sol (take 3 nums) : solve (drop 3 nums)

sol (n:a:b:[]) = let d = b - a
                     as = (a * (n-1)): (a * (n-1) + d) : map (+d) (tail as) in
                     if d == 0 then [a * (n-1)]
                     else take n as
main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM (3 * n) getLine
  let
    nums = map (\x -> read x :: Int) str
    ans = map (\x -> unwords $ map show (sort x)) (solve nums)
  mapM_ putStrLn ans
