import Control.Monad
import qualified Data.Vector as V

mchoose :: (Int, Int) -> Integer
mchoose (n,k) = (V.fromList (map (choose n) [0..k]) V.!) k where
  choose _ 0 = 1
  choose a b = div (mchoose (a, b-1) * (x-y+1)) y where
    x = toInteger a
    y = toInteger b

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    str <- getLine
    let [n,k] = map read $ words str
    print $ mod (mchoose (n,k)) 100000007
