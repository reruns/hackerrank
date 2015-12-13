import qualified Data.Map as M
import Data.List
import Data.Ord
import Data.List.Split
import Control.Monad

trans key = M.fromList $ zip (concat $ sortBy (comparing head) $ zipWith (:) k (transpose $ chunk (length k) $ ['A'..'Z'] \\ k)) ['A'..'Z']
  where k = nub key
solve key text = map (\x -> M.findWithDefault ' ' x (trans key)) text

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    key <- getLine
    text <- getLine
    print $ solve key text
