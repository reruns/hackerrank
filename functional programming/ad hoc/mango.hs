import Data.List
mango a h n m = mangs [1..n] where
                mangs [x] = x
                mangs xs | s > m = mangs right
                         | otherwise = mangs left
                      where
                        mid = div (length xs) 2
                        (left, right) = splitAt mid xs
                        k = head right
                        s = foldl1 (+) $ take k $ sort $ zipWith (+) a $ map ((k-1)*) h

main = do
  [cstr, astr, hstr] <- sequence [getLine, getLine, getLine]
  let
    [n,m] = map read $ words cstr
    a = map read $ words astr
    h = map read $ words hstr
    ans = mango a h n m
  print ans
