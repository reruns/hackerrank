kaprekar 1 = True
kaprekar x = kap (x*x) (10^(numlength x)) where
  kap a z | (div a z)+(mod a z) == x = True
          | otherwise = False

numlength x = nl 10 1 where
  nl a b = if a > x then b else nl (a*10) (b+1)

solve a b = let z = [x | x <- [a..b], kaprekar x] in
            if z == []
            then "INVALID RANGE"
            else unwords $ map show z

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  putStrLn $ solve a b
