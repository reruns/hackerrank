import Data.List
import GHC.Float
import Text.Printf
import System.IO

left (x1,y1) (x2,y2) (x3,y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

solve points = perim $ graham [p] ccw
              where p = minimumBy botLeft points
                    ps = delete p points
                    ccw = (sortBy (sortAngle p) ps)

graham (x:xs) (y:z:rsts)
  | left x y z > 0 = graham (y:x:xs) (z:rsts)
  | left x y z == 0 = graham (x:xs) (z:rsts)
  | otherwise = graham xs (x:z:rsts)
graham xs [z] = z:xs

perim points = foldl1 (+) $ zipWith distance points (tail $ cycle points)

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x_1, y_1) (x_2, y_2) = float2Double $ sqrt $ (x2 - x1)**2 + (y2 - y1)**2
                                 where x1 = fromIntegral x_1
                                       y1 = fromIntegral y_1
                                       x2 = fromIntegral x_2
                                       y2 = fromIntegral y_2

angle :: (Int, Int) -> (Int, Int) -> Double
angle (x_1, y_1) (x_2, y_2) = if a < 0 then (pi + a)
                              else a
                              where x1 = fromIntegral x_1
                                    y1 = fromIntegral y_1
                                    x2 = fromIntegral x_2
                                    y2 = fromIntegral y_2
                                    a = atan ((y2 - y1) / (x2 - x1))
sortAngle p q1 q2
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare (distance p q1) (distance p q2)
  where a1 = angle p q1
        a2 = angle p q2

botLeft (x1, y1) (x2, y2)
  | y1 < y2 = LT
  | y1 > y2 = GT
  | y1 == y2 = compare x1 x2

main :: IO ()
main = do
  withFile "input.txt" ReadMode (\handle -> do
    content <- hGetContents handle
    let
      points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
      ans = solve points
    print ans)
