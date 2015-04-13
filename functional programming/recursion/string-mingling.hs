mingle [] _ = []
mingle (p:ps) (q:qs) = p:q:(mingle ps qs)

main = do
  p <- getLine
  q <- getLine
  putStrLn $ mingle p q
