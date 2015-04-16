smith x | isPrime x = 0
        | digitsum x == factorsum x = 1
        | otherwise = 0

factorsum x = factorize x 0 [2..] where
  factorize x' tot (y:ys) | isPrime x' = tot + (digitsum x')
                          | (mod x' y) == 0 = factorize (div x' y) (tot+(digitsum y)) (y:ys)
                          | otherwise = factorize x' tot ys

digitsum x = foldl1 (+) (digs x) where
  digs 0 = []
  digs x = x `mod` 10 : digs (x `div` 10)

main = do
  n <- readLn :: IO Int
  print $ smith n
  
--fast prime finding
isPrime x | x < 2 = False
          | x < 4 = True
          | otherwise = all (millerRabinPrimality x) mrs where
            mrs | x < 2047 = [2]
                | x < 1373653 = [2,3]
                | x < 9080191 = [31, 73]
                | x < 25326001 = [2,3,5]
                | x < 4759123141 = [2, 7, 61]
                | x < 1122004669633 = [2,13,23,1662803]
                | x < 2152302898747 = [2,3,5,7,11]
                | x < 3474749660383 = [2,3,5,7,11,13]
                | x < 341550071728321 = [2,3,5,7,11,13,17]
                | x < 3825123056546413051 = [2,3,5,7,11,13,17,19,23]

--From haskellwiki:
-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2

-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 =
        error $ "millerRabinPrimality: a out of range ("
              ++ show a ++ " for "++ show n ++ ")"
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs

-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x

mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a

-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)
