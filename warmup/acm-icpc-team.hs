import Control.Monad

score x y = foldl (\y x -> if x then y+1 else y) 0 $ zipWith (\x y -> (x == '1') || (y == '1') ) x y

fldf x y z | (score x z) > (fst y)  = (score x z, 1)
           | (score x z) == (fst y) = (fst y, snd y + 1)
           | otherwise = y

sol (x:[]) = (0,0)
sol (x:xs) = foldl (fldf x) (sol xs) xs

main :: IO ()
main = do
  nm <- getLine
  let n = read (head $ words nm) :: Int
  str <- replicateM n getLine
  let
    ans = sol $ str
  print $ fst ans
  print $ snd ans
