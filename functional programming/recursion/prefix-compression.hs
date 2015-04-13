import Control.Monad

prefix x y = map (\x -> (show $ length x) ++ ' ':x) $ pre "" x y where
             pre p [] ys = [reverse p, [], ys]
             pre p xs [] = [reverse p, xs, []]
             pre p (x:xs) (y:ys) =
               if x /= y then [reverse p, (x:xs), (y:ys)]
               else pre (x:p) xs ys

main = do
  [x,y] <- sequence [getLine, getLine]
  mapM_ putStrLn $ prefix x y
