import Data.List
import qualified Data.Map as Map
import System.IO
import Control.Monad

tuplify [a,b,c] = (a,b,c)

solve :: Int -> [(Int, Int, Int)] -> Int
solve v e = sortBy (\(_,_,a) (_,_,b) -> compare a b) e

checkAdj :: Int -> Int -> Map.Map Int [Int] -> Bool
checkAdj a b ali = f [] [a] where
    f _ [] = False
    f seen (q:qs) | elem b (Map.! ali q) = True
                  | otherwise = f (q:seen) (qs++((Map.! ali q) \\ seen))

main = do
    handle <- openFile "testInput.txt" ReadMode
    cstr <- hGetLine handle
    let [v,e] = map read $ words cstr
    estr <- replicateM e (hGetLine handle)
    let edges = map (\x -> tuplify (map (\y -> read y :: Int) $ words x)) estr
    print $ solve v edges
