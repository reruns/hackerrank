import Control.Monad

solve :: String -> String -> String
solve a b = f 0 a b where
    f c ('1':xs) ('1':ys) | odd c = "NO"
                      | otherwise = f 0 xs ys
    f c ('0':'1':xs) ('1':ys) | odd (c+1) = "NO"
                              | otherwise = f 0 ('1':xs) ys
    f c ('0':xs) ('0':ys) = f c xs ys
    f c ('1':xs) ('0':ys) = f (c+1) xs ys
    f c ('0':xs) ('1':ys) = f (c+1) xs ys
    f c [] _ | odd c = "NO"
             | otherwise = "YES"

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- readLn :: IO Int
        a <- getLine
        b <- getLine
        putStrLn $ solve a b