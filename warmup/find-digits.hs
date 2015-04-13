import Control.Monad

digits = map (read . (:[])) . show

sol [] _ = 0
sol (x:xs) y | x == 0 = sol xs y
               | mod y x == 0 = 1 + sol xs y
               | otherwise = sol xs y

solve x = sol (digits x) x

main :: IO ()
main = do
  t <- readLn :: IO Int
  str <- replicateM t getLine
  let
    nums = map (\x -> read x :: Int) str
    ans = map solve nums
  mapM_ print ans
