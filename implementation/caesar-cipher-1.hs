import Data.List

caesar n c = let l = elemIndex c ['a'..'z']
                 u = elemIndex c ['A'..'Z'] in
             case l of
                Just x -> ['a'..'z'] !! ((x+n) `mod` 26)
                Nothing -> case u of
                                Just y -> ['A'..'Z'] !! ((y+n) `mod` 26)
                                Nothing -> c

main = do
    [n,s,k] <- sequence [getLine, getLine, getLine]
    let kn = read k
    putStrLn $ map (caesar kn) s