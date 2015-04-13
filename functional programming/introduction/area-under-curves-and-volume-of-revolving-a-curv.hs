solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = map (0.001*) [foldl1 (+) ys, foldl1 (\z y -> z + (3.14159 * y**2)) ys]
                where f x = foldl1 (+) $ zipWith (*) (map fromIntegral a) $ map (x ** ) (map fromIntegral b)
                      lef = fromIntegral l
                      rig = fromIntegral r
                      ys = map f [lef, lef+0.001 .. rig]
