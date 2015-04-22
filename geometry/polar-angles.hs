import Data.List
import Control.Monad
import GHC.Float

solve points = unlines $ map pairToNaked (sortBy (sortAngle (0,0)) points)
pairToNaked (x,y) = unwords [show x, show y]

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x_1, y_1) (x_2, y_2) = float2Double $ sqrt $ (x2 - x1)**2 + (y2 - y1)**2
                                 where x1 = fromIntegral x_1
                                       y1 = fromIntegral y_1
                                       x2 = fromIntegral x_2
                                       y2 = fromIntegral y_2
                                       
ang0 :: (Int, Int) -> Double
ang0 (x,y) | x < 0 = pi + a
           | y < 0 = 2 * pi + a
           | otherwise = a
             where x1 = fromIntegral x
                   y1 = fromIntegral y
                   a = atan (y1 / x1)

sortAngle p q1 q2
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare (distance p q1) (distance p q2)
  where a1 = ang0 q1
        a2 = ang0 q2

main :: IO ()
main = do
  t <- readLn :: IO Int
  ps <- replicateM t $ do
    s <- getLine
    let [x,y] = map read $ words s
    return (x,y)
  putStrLn $ solve ps
