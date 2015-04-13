import Data.List
import Data.Ord
import qualified Data.Map as M

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
data Die = Die Int Int Int deriving (Show)

down :: Die -> Die
down (Die top front right) = Die (7-front) top right

right :: Die -> Die
right (Die top front right) = Die (7-right) front top

val :: Die -> Int
val (Die top _ _) = top

-- mdice (x,y) = (M.fromList (zip space $ map dice space) M.!) (x,y) where
--   space = [(a,b) | a <- [1..x], b <- [1..y]]
dice (1,1) = (1, Die 1 2 4)
dice (a,b) | a == 1 = dright
           | b == 1 = ddown
           | otherwise = maximumBy (comparing fst) [dright, ddown]
  where dright = (fst r + (val $ right $ snd r), right $ snd r)
        ddown = (fst d + (val $ down $ snd d), down $ snd d)
        d = dice(a-1,b)
        r = dice(a, b-1)
