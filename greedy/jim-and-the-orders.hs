import Control.Monad
solve os = map head $ sortBy (comparing last) $ zip [1..] (map (\x -> (head x) + (last x)) os)

main = do
  n <- getLine
  ostrs <- replicateM n getLine
  let os = map (map read $ words) ostrs
  print $ solve os
