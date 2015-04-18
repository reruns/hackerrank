import Data.List
import Control.Monad

solve m n = f 1 1 (reverse$sort m) (reverse$sort n) 0 where
  f _ _ [] [] s = s
  f a b [] (y:ys) s = f a b [] ys (s+b*y)
  f a b (x:xs) [] s = f a b xs [] (s+a*x)
  f a b (x:xs) (y:ys) s | a*x > b*y = f a (b+1) xs (y:ys) (s+a*x)
                        | otherwise = f (a+1) b (x:xs) ys (s+b*y)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    [astr,bstr,cstr] <- sequence [getLine, getLine, getLine]
    let xs = map read $ words bstr
        ys = map read $ words cstr
    print $ mod (solve xs ys) 100000007
