import Control.Monad

isFib x = if psqr (5 * x * x + 4) || psqr(5 * x * x - 4) then "IsFibo"
          else "IsNotFibo"

psqr n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)

main :: IO ()
main = do
  t <- readLn :: IO Int
  str <- replicateM t getLine
  let
    nums = map (\x -> read x :: Integer) str
    ans = map isFib nums
  mapM_ putStrLn ans
