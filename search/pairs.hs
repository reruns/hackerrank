import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.HashTable.ST.Basic as H
import Data.Maybe

solve str n k = let nums = words str in do
  runST $ do
    ht <- H.newSized (fromInteger n)
    c <- newSTRef 0
    forM_ nums $ \num -> do
      let x = read num :: Integer
      y <- H.lookup ht (x-k)
      z <- H.lookup ht (x+k)
      --I don't know if we can even do this.
      H.insert ht x True
      modifySTRef c (+(f y z))
    readSTRef c

f x y | a && b = 2
      | a || b = 1
      | otherwise = 0
      where a = not$isNothing x
            b = not$isNothing y

main = do
  cstr <- getLine
  let [n,k] = map (\x -> read x :: Integer) $ words cstr
  nums <- getLine
  print $ solve nums n k
