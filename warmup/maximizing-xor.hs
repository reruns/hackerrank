import Data.Bits

pairs l r = [(a,b) | a <- [l..r], b <- [l..r], a <= b]

maxXor :: Int -> Int -> Int
maxXor l r = foldl (\x y-> max x $ xor (fst y) (snd y)) 0 (pairs l r)

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r
