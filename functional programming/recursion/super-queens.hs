import Data.List
import qualified Data.Vector as Vec

q n = queens n (Vec.fromList []) 1 0

queens n xs y q | Vec.length xs == n = queens n (Vec.drop 1 xs) (1 + Vec.head xs) (q+1)
                | y > n = if Vec.length xs == 0 then q
                  else queens n (Vec.drop 1 xs) (1 + Vec.head xs) q
                | conflicts 1 xs y = queens n xs (y+1) q
                | otherwise = queens n (Vec.cons y xs) 1 q

conflicts n v y | n-1 >= Vec.length v = False
                | (y == z || (abs (y - z) == n)) = True
                | (abs (y-z) + n == 3) = True
                | otherwise = conflicts (n+1) v y
                where z = v Vec.! (n-1)

--For the record, here are the total numbers of solutions for super queens
--N : Solutions
--8 : 0
--9 : 0
--10: 4
--11: 44
--12: 156
--13: 1876
--14: 5180
