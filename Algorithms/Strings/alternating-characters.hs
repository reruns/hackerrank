import Control.Monad

shankify (x:[]) = 0
shankify (x:xs) = if (head xs) == x then 1 + shankify xs else shankify xs

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map shankify str
  mapM_ print ans
