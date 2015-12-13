import Control.Monad
import Data.Char

solve xs = foldl f 0 (zoop xs)
f x t = x + (abs $ (ord $ fst t) - (ord $ snd t))
zoop xs = zip (take (div (length xs) 2 + mod (length xs) 2) xs) (reverse xs)

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map solve str
  mapM_ print ans
