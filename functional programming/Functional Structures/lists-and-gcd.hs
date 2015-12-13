import Data.List
import Control.Monad

--inputs are two sorted lists in the given format...
intersect' [] _ = []
intersect' _ [] = []
intersect' (x1:x2:xs) (y1:y2:ys) | x1 == y1 = x1: (min x2 y2): intersect' xs ys
                                 | x1 > y1 = intersect' (x1:x2:xs) ys
                                 | otherwise = intersect' xs (y1:y2:ys)

gcdlist :: [[Int]] -> [Int]
gcdlist xs= foldl1 intersect' xs

main = do
  q <- readLn :: IO Int
  str <- replicateM q getLine
  let nums = map (\x -> map (\y -> read y :: Int) $ words x) str
  putStrLn $ unwords $ map show $ gcdlist nums
