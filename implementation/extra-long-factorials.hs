import Control.Applicative
import Control.Monad
import System.IO

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Integer
    print $ factorial n