import Control.Monad
import System.IO
import qualified Data.Array as A

fparity aas x y | (x /= y) && aas A.! (x+1) == 0 = "Odd"
                | odd $ aas A.! x = "Odd"
                | otherwise = "Even"

main = do
    inh <- openFile "testinput.txt" ReadMode
    outh <- openFile "testoutput.txt" WriteMode
    [n,a,q] <- sequence (replicate 3 (hGetLine inh))
    let nn = read n
        qn = read q
        aas = A.array (1,nn) $ zip [1..] (map read $ words a :: [Int])
    replicateM_ qn $ do
        s <- hGetLine inh
        let [x,y] = map read $ words s
        hPutStrLn outh $ fparity aas x y
    hClose inh
    hClose outh