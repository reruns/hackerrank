import Data.List
import Control.Monad

anapal text = length (elemIndices True $ map (\x -> odd $ length x) $ group $ sort text) <= 1

convert x = if x then "YES" else "NO"

main :: IO ()
main = do
  str <- getLine
  putStrLn $ convert $ anapal str
