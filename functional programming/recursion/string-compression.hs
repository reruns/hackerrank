compress [] _ = []
compress (a:str) n | str == [] = cur
                   | a == head str = compress str (n+1)
                   | n > 1 = cur ++ (compress str 1)
                   | otherwise = a:(compress str 1)
                   where cur = if n > 1 then a:(show n) else a:[]

main = do
  str <- getLine
  putStrLn $ compress str 1
