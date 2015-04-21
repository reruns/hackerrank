solve heights = if a >= 0 then i else i+ fd (-a) (2^(length heights)) where
  i = fd (head heights) 2
  a = head $ f i heights []
  f e [] es = e:es
  f e (h:hs) es = f (2*e-h) hs (e:es)

--it's div, but it rounds up!
fd x y | x `mod` y == 0 = div x y
       | otherwise = 1 + div x y

main = do
  n <- readLn :: IO Int
  hstr <- getLine
  let hs = map read $ words hstr
  print $ solve hs
