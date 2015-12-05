import Control.Applicative
import Control.Monad
import System.IO

fine (d1,m1,y1) (d2,m2,y2) | y1 > y2 = 10000
                           | y1 < y2 = 0
                           | m1 > m2 = 500 * (m1 - m2)
                           | m1 < m2 = 0
                           | d1 > d2 = 15 * (d1-d2)
                           | otherwise = 0
                           
main :: IO ()
main = do
    d1_temp <- getLine
    let d1_t = words d1_temp
    let d1 = read $ d1_t!!0 :: Int
    let m1 = read $ d1_t!!1 :: Int
    let y1 = read $ d1_t!!2 :: Int
    d2_temp <- getLine
    let d2_t = words d2_temp
    let d2 = read $ d2_t!!0 :: Int
    let m2 = read $ d2_t!!1 :: Int
    let y2 = read $ d2_t!!2 :: Int
    print $ fine (d1,m1,y1) (d2,m2,y2)

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          
