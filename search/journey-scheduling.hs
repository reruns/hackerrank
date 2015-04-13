import Control.Monad
import Control.Monad.ST
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Array.ST
import Data.List
import Data.Ord
import Data.Tuple

graphify es = runSTArray $ do
  arr <- newArray (1, 1+(toInteger$length es)) []
  forM_ es $ \e -> do
    let [a,b] = map (\x -> read x :: Integer) $ words e
    t <- readArray arr a
    k <- readArray arr b
    writeArray arr a (b:t)
    writeArray arr b (a:k)
  return arr

ls g v = search [v] [] 0 where
  search vs seen n = if cs==[] then (zip vs (repeat n)) else (zip vs (repeat n)) ++ search cs vs (n+1) where
    cs = filter (\x -> not$elem x seen) (concatMap (g A.!) vs)

eccentricity g v = (mm g) M.! v
diameter g = M.fold max 0 (mm g)

--performance note: this requires sorting two lists of size k
--if k is 10^5, this is going to be obnoxious.
mm g = M.fromList $ zipWith f (sortBy (comparing fst) a) (sortBy (comparing fst) b) where
  s = fst $ last $ ls g 1
  a = ls g s
  e = fst $ last a
  b = ls g e
  f (w,x) (y,z) = (w, max x z)

main = do
  cstr <- getLine
  let [n,m] = map (\x -> read x :: Int) $ words cstr
  pairs <- replicateM (n-1) getLine
  let g = graphify pairs
      d = diameter g
  replicateM_ m $ do
    vstr <- getLine
    let [v,k] = map (\x -> read x :: Integer) $ words vstr
    print $ (k-1)*d + eccentricity g v
