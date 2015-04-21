import qualified Data.Map as M

--This is wrong!
--Probably because the test cases have numbers already in place
--which this doesn't deal with properly.
solve n k xs = let a = reverse [(n-k+1)..n]
                   b = M.fromList $ zip a (take k xs)
                   f x = if x > n-k then b M.! x else x in
               a ++ (map f (drop k xs))

main = do
  s1 <- getLine
  s2 <- getLine
  let [n,k] = map read $ words s1
      xs = map read $ words s2
  print $ solve n k xs
