import Data.List
import Data.Ord
import qualified Data.Vector as V
import Control.Monad

solve vertices edges = V.foldl g 0 t where
    t = V.fromList $ f es 1 vertices
    es = groupBy (\x y -> (snd x) == (snd y)) $ sortBy (comparing snd) edges
    g z xs | (fst y) == 0 = z + snd y
           | odd (fst y) = z + snd y
           | otherwise = z where
           y = foldl parity (0,0) xs
           parity (a,b) w = if odd (children w) then (a+1, b) else (a, b+1)
           children node = (foldl (\z w -> z + children w) 1 (t V.! (node-1)))

f :: [[(Int, Int)]] -> Int -> Int -> [[Int]]
f [] n v | n > v = []
         | otherwise = []:f [] (n+1) v
f (x:xs) n v | n > v = []
             | snd (head x) == n = (map fst x):f xs (n+1) v
             | otherwise = []:f (x:xs) (n+1) v

tuplify [a,b] = (a,b)

main = do
    cstr <- getLine
    let [v,e] = map read $ words cstr
    estr <- replicateM e getLine
    let edges = map (\x -> tuplify (map (\y -> read y :: Int) $ words x)) estr
    print $ solve v edges
