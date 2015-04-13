--this is totally wrong, duh.


-- sol x = f x 0 1
--
-- f 0 s c = s
-- f a s c = f (a-1) (s+c') c' where
--   c' = c*a
--
-- ans string = sol $ foldl (\z x-> z + div (length x) 2) 0 (group$sort string)
--
-- main = do
--   t <- readLn :: IO Int
--   replicateM_ t $ do
--     s <- getLine
--     print $ ans s
