import Control.Monad

solve (x:[]) = False
solve (x:xs) = any (\z -> 1 == (gcd x z)) xs || (solve xs)

edrop [] p = []
edrop (x:xs) p = if p then x : edrop xs (not p) else edrop xs (not p)

main :: IO ()
main = do
  t <- readLn :: IO Int
  str <- replicateM (2*t) getLine
  let
    lists = map (\x -> map (\y -> read y :: Int) $ words x) (edrop str False)
    ans = map (\x -> if solve x then "YES" else "NO") lists
  mapM_ putStrLn ans
