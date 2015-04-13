import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map (\x -> read x :: Int) str
  mapM_ print ans
