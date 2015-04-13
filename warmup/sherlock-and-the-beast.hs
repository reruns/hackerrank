import Control.Monad

decent x | x < 3 = (-1, -1)
         | x == 3 = (3, 0)
         | x == 5 = (0, 5)
         | fst thr /= -1 = (3 + (fst thr), snd thr)
         | fst fiv /= -1 = (fst fiv, 5 + (snd fiv))
         | otherwise = (-1,-1)
         where thr = decent (x-3)
               fiv = decent (x-5)

solve x = let d = decent x in
          if fst d == -1 then "-1"
          else take (fst d) (repeat '5') ++ take (snd d) (repeat '3')

main :: IO ()
main = do
  t <- readLn :: IO Int
  str <- replicateM t getLine
  let
    nums = map (\x -> read x :: Int) str
    ans = map solve nums
  mapM_ putStrLn ans
