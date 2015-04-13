import Control.Monad

squares a b | a * a > b = 0
            | otherwise = 1 + squares (a+1) b

numerify (x:[]) = [map (\y -> read y :: Int) $ words x]
numerify (x:xs) = (map (\y -> read y :: Int) $ words x) : (numerify xs)

solve xs = squares (ceiling $ sqrt $ fromIntegral $ head xs) (last xs)

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map solve $ numerify str
  mapM_ print ans
