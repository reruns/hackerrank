import Control.Monad

numerify (x:[]) = [map (\y -> read y :: Int) $ words x]
numerify (x:xs) = (map (\y -> read y :: Int) $ words x) : (numerify xs)

fldf t (x:y:z:[]) = t + (y - x + 1) * z

sol :: [String] -> Int -> Int
sol a b = div (foldl fldf 0 (numerify a)) b

main :: IO ()
main = do
  nm <- getLine
  let n = read (head $ words nm) :: Int
      m = read (last $ words nm) :: Int
  str <- replicateM m getLine
  let
    ans = sol str n
  print ans
