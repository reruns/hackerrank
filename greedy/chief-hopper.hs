solve heights = if a > 0 then i else 1+i+ div (-a) (2^(length heights)) where
  i = fd $ head heights
  a = head $ f i heights []
  f e [] es = e:es
  f e (h:hs) es = f (2*e-h) hs (e:es)

--it's div, but it rounds up!
fd x | even x = div x 2
     | otherwise = 1+ div x 2

main = do
  n <- readLn :: IO Int
  hstr <- getLine
  let hs = map read $ words hstr
  print $ solve hs
