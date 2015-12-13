import Control.Monad

bnot 0 = 1
bnot x = 0

flipwith x y = bnot (div x y) * y

bflip x 1 = bnot x
bflip x y = flipwith x y + bflip (mod x y) (div y 2)

bf32 x = bflip x 2147483648

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    num = map (\x -> read x :: Int) str
    ans = map bf32 num
  mapM_ print num
