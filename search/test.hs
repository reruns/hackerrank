import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Tuple

graphify es = runSTArray $ do
  arr <- newArray (1, 1+length es) []
  forM_ (es ++ (map swap es)) $ \e -> do
    t <- readArray arr (fst e)
    writeArray arr (fst e) ((snd e):t)
  return arr

primesUpto :: Int -> [Int]
primesUpto n = [p | (p, True) <- assocs $ sieve n]

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    sieve <- newArray (2, n) True
    forM_ [2..n] $ \p -> do
        isPrime <- readArray sieve p
        when isPrime $ do
            forM_ [p*2, p*3 .. n] $ \k -> do
                writeArray sieve k False
    return sieve
