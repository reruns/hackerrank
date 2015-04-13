import Control.Monad

numerify (x:[]) = [map (\y -> read y :: Int) $ words x]
numerify (x:xs) = (map (\y -> read y :: Int) $ words x) : (numerify xs)

sol (n:c:m:[]) = k + div (k-1) (m-1)
                 where k = div n c

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map sol $ numerify str
  mapM_ print ans
