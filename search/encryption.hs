import Data.List.Split
import Data.List

encrypt text = unwords $ transpose $ chunk c text where
  l = length text
  r = (floor . sqrt . fromIntegral) l
  c = (ceiling . sqrt . fromIntegral) l

main = do
  text <- getLine
  putStrLn $ encrypt text
