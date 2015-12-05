import Control.Applicative
import Control.Monad
import System.IO

time h m | m == 0 = (wordify h) ++ " o' clock"
         | m == 15 || m == 30 = (wordify m) ++ " past " ++ (wordify h)
         | m < 30 = (wordify m) ++ " minutes past " ++ (wordify h)
         | m == 45 = (wordify (60-m)) ++ " to " ++ (wordify (h+1))
         | otherwise = (wordify (60-m)) ++ " minutes to " ++ (wordify (h+1))

         
wordify x = ["one","two","three","four","five","six","seven","eight","nine","ten",
           "eleven","twelve","thirteen","fourteen","quarter", "sixteen","seventeen",
           "eighteen","nineteen", "twenty", "twenty one", "twenty two", "twenty three",
           "twenty four", "twenty five", "twenty six", "twenty seven", "twenty eight",
           "twenty nine", "half"] !! (x-1)

main :: IO ()
main = do
    h_temp <- getLine
    let h = read h_temp :: Int
    m_temp <- getLine
    let m = read m_temp :: Int
    putStrLn $ time h m