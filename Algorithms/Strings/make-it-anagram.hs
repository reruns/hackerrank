import Data.List

solve a b | q > p =  2 * length (a \\ b) - q + p
          | q < p = 2 * length (b \\ a) - p + q
          | otherwise = 2 * length (a \\ b) where
          q = length a
          p = length b

main = do
  a <- getLine
  b <- getLine
  print $ solve a b
