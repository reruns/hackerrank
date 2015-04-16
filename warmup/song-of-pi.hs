import Control.Monad

pil = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2,6,4,3,3,8,3,3]

solve song = let w = map length (words song) in
  if w == (take (length w) pil)
  then "It's a pi song."
  else "It's not a pi song."

main = do
  n <- readLn :: IO Int
  replicateM_ n $ do
    str <- getLine
    putStrLn $ solve str
