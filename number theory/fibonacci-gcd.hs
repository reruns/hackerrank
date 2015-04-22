import Control.Monad
import Data.List

data Matrix = Matrix { x1 :: Integer, x2 :: Integer, y1 :: Integer, y2 :: Integer} deriving (Show)

m = 10^9 + 7
--2D matrix multiplication
multiply :: Matrix -> Matrix -> Matrix
multiply x y = Matrix a b c d where
  a = (x1 x * x1 y) + (x2 x * y1 y)
  b = (x1 x * x2 y) + (x2 x * y2 y)
  c = (y1 x * x1 y) + (y2 x * y1 y)
  d = (y1 x * x2 y) + (y2 x * y2 y)

--exponentiation for the matrix type
matexp :: Matrix -> Integer -> Matrix
matexp a 1 = a
matexp a n | odd n = multiply (matexp (multiply a a) (div n 2)) a
           | otherwise = matexp (multiply a a) (div n 2)

fib = mfib 0 1
mfib a b n | n == 0 = a
           | n == 1 = b
           | otherwise = (b * (x1 fn) + a * (x2 fn)) where
           fn = matexp fib0 (n-1)

fib0 = Matrix 1 1 1 0

--basically a fold with early termination
--tracking dupes with a Hashtable is faster than nub
--but nub is faster than nothing, so let's try it.
solve (a:as) = f (fib a) (nub as) `mod` m where
  f x [] = x
  f 1 _ = 1
  f x (y:ys) = f (gcd x (fib y)) ys

main = do
  t <- readLn :: IO Int
  s <- replicateM t getLine
  let as = map (\x -> read x :: Integer) s
  print $ solve as
