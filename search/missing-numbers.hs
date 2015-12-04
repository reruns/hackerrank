import qualified Data.IntMap.Strict as I
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UV
import Data.Int
import System.IO
import Control.Monad.State

--Thanks to 
--see: http://blog.malde.org/posts/frequency-counting.html

--Strict IntMaps aren't quite fast enough
freqCount_I :: [Int] -> (I.IntMap Int)
freqCount_I = I.fromListWith (+) . map (\x -> (x,1::Int))
    
compareMap a b = I.foldrWithKey f [] b where
    f k bc r = let ac = I.findWithDefault 0 k a in
              if ac /= bc then (k:r) else r

--gotta go fast.
freqCount_UV v xs min = do 
  v0 <- UV.replicate (v+1) 0
  mapM_ (\x -> insV v0 (x-min)) xs
  v1 <- U.freeze v0
  return v1
  
insV :: UV.IOVector Int -> Int -> IO ()
insV v x = do
  val <- UV.unsafeRead v x  -- NSFT!
  UV.unsafeWrite v x (val+1)
  
compareVec a b = (reverse.snd) $ U.foldl f (0,[]) a where
    f (k,r) v | (b U.! k) /= v = (k+1, k:r)
              | otherwise = (k+1,r)
              
main = do
    --[n,a,m,b] <- sequence [getLine, getLine, getLine, getLine]
    putStrLn "getting file input"
    handle <- openFile "testinput.txt" ReadMode
    [n,a,m,b] <- sequence $ replicate 4 (hGetLine handle)
    hClose handle
    putStrLn "translating to lists"
    let an = map read $ words a
        bn = map read $ words b
    putStrLn "computing min"
    let min = minimum bn
    putStrLn "counting a"
    av <- freqCount_UV 101 an min
    putStrLn "counting b"
    bv <- freqCount_UV 101 bn min
    putStrLn "comparing"
    let out = map (min+) $ compareVec av bv
    putStrLn "done!"
    putStrLn $ unwords (map show out)