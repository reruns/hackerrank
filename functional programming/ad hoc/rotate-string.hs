import Data.List
import Control.Monad

rots xs = take (length xs) $ tail $ (zipWith (++) (tails xs) (inits xs))

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    str <- getLine
    putStrLn $ unwords $ rots str
