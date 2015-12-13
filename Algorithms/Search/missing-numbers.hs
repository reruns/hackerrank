import qualified Data.IntMap.Lazy as I
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UV
import Data.Array
import System.IO


--see: http://blog.malde.org/posts/frequency-counting.html

--Strict IntMaps aren't quite fast enough
freqCount_I :: [Int] -> (I.IntMap Int)
freqCount_I = I.fromListWith (+) . map (\x -> (x,1::Int))
    
compareMap a b = I.foldrWithKey f [] b where
    f k bc r = let ac = I.findWithDefault 0 k a in
              if ac /= bc then (k:r) else r

--gotta go fast.
freqCount_UV v als bes = do 
  v0 <- UV.replicate (v+1) 0
  mapM_ (\x -> insV v0 x) bes
  mapM_ (\x -> invV v0 x) als
  v1 <- U.freeze v0
  return v1
  
insV :: UV.IOVector Int -> Int -> IO ()
insV v x = do
  val <- UV.unsafeRead v x  -- NSFT!
  UV.unsafeWrite v x (val+1)
  
invV :: UV.IOVector Int -> Int -> IO ()
invV v x = do
  val <- UV.unsafeRead v x
  UV.unsafeWrite v x (val-1)
  
compareVec vec = (reverse.snd) $ U.foldl f (0,[]) vec where
    f (k,r) v | v /= 0 = (k+1, k:r)
              | otherwise = (k+1,r)
              
              

main = do
    --[n,a,m,b] <- sequence [getLine, getLine, getLine, getLine]
    contents <- readFile "testinput.txt"
    let [n,a,m,b] = lines contents
        an = map read $ words a :: [Int]
        bn = map read $ words b :: [Int]
    av <- freqCount_UV 10001 an bn
    let out = compareVec av
    print out