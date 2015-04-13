import qualified Data.Vector as Vec
import Control.Monad

solve lanes i j = Vec.minimum (Vec.slice i (j-i+1) lanes)

main :: IO ()
main = do
  cstr <- getLine
  let
    controls = map (\x -> read x :: Int) (words cstr)
    t = last controls
  highway <- getLine
  pairs <- replicateM t getLine
  let
    lanes = Vec.fromList $ map (\x -> read x :: Int) (words highway)
    ijs = map (\x -> map (\y -> read y :: Int) (words x)) pairs
    ans = map (\x -> solve lanes (head x) (last x)) ijs
  mapM_ print ans
