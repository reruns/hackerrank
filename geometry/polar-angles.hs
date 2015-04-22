import Data.List
import Control.Monad
import GHC.Float

--put it all together, with some processing
--most of this would look nice in main, but I prefer this convention
solve points = unlines $ map pairToNaked (sortBy sortAngle points)
pairToNaked (x,y) = unwords [show x, show y]

--A special comparator so we fall back to distance
sortAngle q1 q2
  | a1 == a2 = compare (dist0 q1) (dist0 q2)
  | otherwise = compare a1 a2
  where a1 = ang0 q1
        a2 = ang0 q2

--distance from origin
dist0 :: (Int, Int) -> Double
dist0 (x_1, y_1) = float2Double $ sqrt $ (x1)**2 + (y1)**2 where
              x1 = fromIntegral x_1
              y1 = fromIntegral y_1

--angle with positive x-axis
ang0 :: (Int, Int) -> Double
ang0 (x,y) | x < 0 = pi + a
           | y < 0 = 2 * pi + a
           | otherwise = a
             where x1 = fromIntegral x
                   y1 = fromIntegral y
                   a = atan (y1 / x1)

main :: IO ()
main = do
  t <- readLn :: IO Int
  ps <- replicateM t $ do
    s <- getLine
    let [x,y] = map read $ words s
    return (x,y)
  putStrLn $ solve ps
