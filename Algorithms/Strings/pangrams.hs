import Data.Char
import Data.List

pan text = if sort.nub $ map toLower text == ['a'..'z']
           then "pangram"
           else "not pangram"

main = do
  text <- getLine
  putStrLn $ pan text
