import Data.List

gtake ts k = f ts k 0 where
  f [] _ c = c
  f (t:ts) m c = if t <= m then f ts (m-t) (c+1)
                 else f ts m c

main = do
  cstr <- getLine
  let [n,m] = map read $ words cstr
  nstr <- getLine
  let ts = map read $ words cstr
  print $ gtake (sort ts) m
