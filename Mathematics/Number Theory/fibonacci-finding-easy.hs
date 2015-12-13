import Control.Monad

data Matrix = Matrix { x1 :: Integer, x2 :: Integer, y1 :: Integer, y2 :: Integer} deriving (Show)

m = 10^9 + 7
--2D matrix multiplication
multiply :: Matrix -> Matrix -> Matrix
multiply x y = Matrix a b c d where
  a = ((x1 x * x1 y) `mod` m) + ((x2 x * y1 y) `mod` m)
  b = ((x1 x * x2 y) `mod` m) + ((x2 x * y2 y) `mod` m)
  c = ((y1 x * x1 y) `mod` m) + ((y2 x * y1 y) `mod` m)
  d = ((y1 x * x2 y) `mod` m) + ((y2 x * y2 y) `mod` m)

--exponentiation for the matrix type
matexp :: Matrix -> Integer -> Matrix
matexp a 1 = a
matexp a n | odd n = multiply (matexp (multiply a a) (div n 2)) a
           | otherwise = matexp (multiply a a) (div n 2)


mfib a b n | n == 0 = a
           | n == 1 = b
           | otherwise = (b * (x1 fn) + a * (x2 fn)) `mod` m where
           fn = matexp fib0 (n-1)

fib0 = Matrix 1 1 1 0

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    s <- getLine
    let [a,b,n] = map (\x -> read x :: Integer) $ words s
    print $ mfib a b n
