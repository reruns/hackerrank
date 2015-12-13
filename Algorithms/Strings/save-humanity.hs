import Control.Monad
import Data.List

--this ain't fast.
tcompare a b = f True a b where
    f _ [] [] = True
    f _ [] _ = False
    f _ _ [] = False
    f b (x:xs) (y:ys) | x == y = f b xs ys
                      | x /= y && b = f False xs ys
                      | otherwise = False
                      
solve a b = f [] 0 a where
    l = length b
    f r _ [] = r
    f r n (x:xs) | tcompare (take l (x:xs)) b = f (n:r) (n+1) xs
                 | otherwise = f r (n+1) xs
                      
ans a b = f (solve a b) where
    f [] = "No Match!"
    f xs = unwords.reverse $ map show xs
    
main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        str <- getLine
        let [a,b] = words str
        putStrLn $ ans a b