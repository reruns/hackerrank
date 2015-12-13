import Data.List

solve cs k = foldl (+) $ zipWith (*) (reverse $ sort cs) $ iterate (map (+1)) (replicate k 1)

main = do
  [nstr, cstr] <- sequence [getLine, getLine]
  let [n,k] = map read $ words nstr
      cs = map read $ words cstr
  print $ solve cs k
