import qualified Data.Map as M
import Data.List

--find the directions we can walk in
walk m ((a,b),c) = let l = [x | x <- cardinals (a,b), M.findWithDefault 'X' x m /= 'X'] in
                if length l > 1
                then zip l (repeat (c+1))
                else zip l (repeat c)


cardinals (a,b) = [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]
