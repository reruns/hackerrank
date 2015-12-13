import Control.Monad
import Data.List

isSorted [x] = True
isSorted (x:xs) = if x > (head xs) then False else isSorted xs

--gotta figure out some kinda shortcut here
game xs | isSorted xs = False
        | length xs == 2 = True
        | otherwise = any (not.game) $ map (\x -> delete xs x) [0..length xs-1]

solve xs = if game xs then "Alice" else "Bob"

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- getLine
        str <- getLine
        let xs = map (\x -> read x :: Int) $ words str
        putStrLn $ solve xs
