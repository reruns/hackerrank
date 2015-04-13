import Control.Monad

swap [] = []
swap (a:b:str) = b:a:(swap str)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    str <- getLine
    putStrLn $ swap str
