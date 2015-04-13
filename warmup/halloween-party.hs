import Control.Monad

sol k = let c = div k 2 in
        c * (c + (mod k c))

main :: IO ()
main = do
  t <- readLn :: IO Int
  str <- replicateM t getLine
  let
    nums = map (\x -> read x :: Int) str
    ans = map sol nums
  mapM_ print ans
