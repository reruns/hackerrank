search v xs = find v xs 0
              where find v (x:xs) n = if v == x then n else find v xs (n+1)

main :: IO ()
main = do
  v <- readLn :: IO Int
  n <- readLn :: IO Int
  arstr <- getLine
  let
    nums = map (\x -> read x :: Int) $ words arstr
    ans = search v nums
  print ans
