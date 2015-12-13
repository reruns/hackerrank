import Control.Monad

palidx text = test text (reverse text) 0 where
  test [] _ _ = -1
  test _ [] _ = -1
  test (a:as) (b:bs) n = if a == b then test as bs (n+1)
                         else if left==(reverse left) then n
                         else (length text) - n - 1 where
    left = (take n text) ++ (drop (n+1) text)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    text <- getLine
    print $ palidx text
