import Control.Monad
import Data.List
import Data.Ord

subsum xs ys = sbs xs ys 1 where
               sbs [] ys _ = zip (map fst ys) (repeat (-1))
               sbs _ [] _ = []
               sbs (x:xs) ((a,b):ys) n | x >= b  = (a,n) : sbs (x:xs) ys n
                                   | otherwise = sbs xs ((a,b):ys) (n+1)

main = do
  n <- readLn :: IO Int
  astr <- getLine
  t <- readLn :: IO Int
  let a = scanl1 (+) $ reverse $ sort $ map (\x -> read x :: Integer) $ words astr
  s <- replicateM t (readLn :: IO Integer)
  let ss = sortBy (comparing snd) $ zip [1..] s
      sss = subsum a ss
      ans = map snd $ sortBy (comparing fst) sss
  mapM_ print ans
