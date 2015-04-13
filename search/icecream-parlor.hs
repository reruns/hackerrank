import Control.Monad
import Data.List
import Data.Maybe

icecream a (f:fs) j = let k = elemIndex (a-f) fs in
  if k == Nothing then icecream a fs (j+1)
  else (show j) ++ " " ++ (show $ (fromJust k) + j + 1)

main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    m <- readLn :: IO Int
    n <- getLine
    str <- getLine
    let fs = map read $ words str
    putStrLn $ icecream m fs 1
