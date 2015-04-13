import qualified Data.Vector as V
import Data.Tuple
import Control.Monad

inorder v = unwords $ out 1 where
  out (-1) = []
  out n = (out $ fst c)++(show n):(out $ snd c) where
    c = v V.! (n-1)

swaps v k = swoop 1 1 where
  swoop _ (-1) = []
  swoop h n = if 0==mod h k then n:subs else subs where
    childs = v V.! (n-1)
    subs = concatMap (swoop (h+1)) [fst childs, snd childs]

solve v ks = unlines.tail $ V.toList $ V.map inorder $ V.scanl process v (V.fromList $ map (swaps v) ks)
    where process nv ss = (nv V.// (map (\s -> (s-1,swap $ nv V.! (s-1))) ss))

tuplify s = (a,b) where
  [a,b] = map read $ words s

main = do
  n <- readLn :: IO Int
  nodes <- sequence $ replicate n getLine
  let v = V.fromList $ map tuplify nodes
  t <- readLn :: IO Int
  ks <- replicateM t (readLn :: IO Int)
  putStrLn $ solve v ks
