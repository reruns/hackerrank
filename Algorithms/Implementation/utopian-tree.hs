import Control.Monad
tree x | x == 0 = 1
       | even x = tree (x-1) + 1
       | otherwise = 2 * tree (x-1)

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    num = map (\x -> read x :: Int) str
    ans = map tree num
  mapM_ print ans
