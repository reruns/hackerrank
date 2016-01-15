import System.IO

--Tail recursion is very important.
solve :: String -> Integer
solve n = f (reverse n) 0 0 0  where
    f :: String -> Integer -> Integer -> Int -> Integer
    f [] s _ _ = s
    f (n:ns) s ts i = let nt = (ts * 10 + 1) `mod` m
                          num = read [n] in
                      f ns (s + (num * nt * (toInteger $ l-i)) `mod` m) nt (i+1)     
    l = length n
  
m = 10^9 + 7  
main = do
    h <- openFile "testinput.txt" ReadMode
    n <- hGetContents h
    print $ (solve n) `mod` m