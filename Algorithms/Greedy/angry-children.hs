import Control.Monad
import Data.List
import qualified Data.Vector as Vec

dcheck k xs ix |Vec.length xs == k + ix +1  = abs $ ((Vec.!) xs ix) - ((Vec.!) xs (ix+k-1))
               |otherwise = min (dcheck k xs (ix+1)) (abs $ ((Vec.!) xs ix) - ((Vec.!) xs (ix+k-1)))

main :: IO ()
main = do
  n <- readLn :: IO Int
  k <- readLn :: IO Int
  str <- replicateM n getLine
  let
    nums = map (\x -> read x :: Integer) str
    ans = dcheck k (Vec.fromList $ sort nums) 0
  print ans
