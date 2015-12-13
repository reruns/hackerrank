import Control.Monad

solve arr = if c /= 0 then (show b) ++ " " ++ (show c)
            else (show $ maximum arr) ++ " " ++ (show $ maximum arr)
  where (a,b,c) = foldl f (0,0,0) arr

f (x,y,z) n = (a,b,c) where
    a = if x > 0 then x + n else n
    b = if a > y then a else y
    c = if n > 0 then n + z else z

main = do
    t <- readLn :: IO Int
    replicateM_ t $ do
        n <- getLine
        str <- getLine
        let nums = map read $ words str
        putStrLn $ solve nums
