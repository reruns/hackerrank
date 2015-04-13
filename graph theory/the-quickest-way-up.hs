import qualified Data.Map as M
import Control.Monad
import Data.List

snakes paths = find 0 [1] where
  find c ns = if any (100==) ns then c
              else find (c+1) $ nub $ concat $ zipWith (\x y -> jump (x+y)) ns [1..6]
  jump n = M.findWithDefault n n paths


tuplify str = (a,b) where
  a = read (fst x) :: Int
  b = read (tail $ snd x) :: Int
  x = span (/= ',') str

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    cstr <- getLine
    lads <- getLine
    snak <- getLine
    let paths = map tuplify (words (lads ++ " " ++ snak))
    print $ snakes (M.fromList paths)
