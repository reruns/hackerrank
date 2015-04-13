import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V

solve n m a b c = let factors = V.accum mulMod (V.replicate (m+1) 1) $ zip b c
                      indexedFactors = zip [1..] $ tail $ V.toList factors
                      d = concat [map (\x -> (x-1, f)) [i, i+i .. n] | (i, f) <- indexedFactors] in
                  V.toList $ V.accum mulMod (V.fromList a) d

mulMod x y     = x * y `mod` 1000000007

main :: IO ()
main = do
  let readInt s = let Just (i,_) = B.readInt s in i
  inp <- fmap (map (map readInt . B.words) . B.lines) B.getContents
  let [[n,m],a,b,c]  = inp :: [[Int]]
  putStrLn $ unwords $ map show $ solve n m a b c
