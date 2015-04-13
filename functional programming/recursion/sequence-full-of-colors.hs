import Control.Monad

colors s = col s 0 0 0 0 where
           col [] r g y b = if r == g && y == b then True else False
           col (x:xs) r g y b | abs (y - b) > 1 = False
                              | abs (r - g) > 1 = False
                              | otherwise = case x of
                                'R' -> col xs (r+1) g y b
                                'G' -> col xs r (g+1) y b
                                'Y' -> col xs r g (y+1) b
                                'B' -> col xs r g y (b+1)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    s <- getLine
    print $ colors s
