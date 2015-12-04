import qualified Data.Map as M

freqCount :: [Char] -> (M.Map Char Int)
freqCount = M.fromListWith (+) . map (\x -> (x,1::Int))

isValid count = f True (tail zs) where
                zs = M.elems count
                n = head zs
                f _ [] = "YES"
                f b (x:xs) | x == n = f b xs 
                           | x == (n+1) && b = f False xs 
                           | x == 1 && b = f False xs
                           | otherwise = "NO"
                           
main = do
    s <- getLine
    putStrLn $ (isValid.freqCount) s