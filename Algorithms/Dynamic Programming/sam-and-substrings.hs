import Data.List
import qualified Data.Map as M

mcandy n = (M.fromList (map (\x -> (x,candy (show x))) [0..])) M.! n

candy :: String -> Int
candy m = if (read m) <= 9
          then (read m)
          else (read m) + (mcandy (tail m)) + (mcandy (init m))
    
main = do
    n <- readLn :: IO Int
    print $ mcandy n