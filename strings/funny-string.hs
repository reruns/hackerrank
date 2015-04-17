import Data.Char
import Control.Monad

cdifs (a:[]) = []
cdifs (a:b:cs) = (abs$(ord a)-(ord b)):cdifs (b:cs)

isPalindrome x = x == reverse x

funny str = if isPalindrome $ cdifs str then "Funny" else "Not Funny"

main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    str <- getLine
    putStrLn $ funny str
