import Control.Monad

main = do
    istr <- getLine
    let [n,k] = map (\x -> read x :: Int) $ words istr
    print n