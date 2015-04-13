import Data.List
import Control.Monad

solve a b k = if all (>=k) $ zipWith (+) (sort a) (reverse $ sort b) then "YES" else "NO"

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    [cstr,astr,bstr] <- sequence [getLine, getLine, getLine]
    let [n,k] = map read $ words cstr
        a = map read $ words astr
        b = map read $ words bstr
    putStrLn $ solve a b k
