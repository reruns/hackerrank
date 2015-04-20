solve x = foldl (\z y -> z*(1+2*rdiv x y) `mod` 1000007) 1 (takeWhile (<=x) primes)

--rdiv calculates the multiplicity of a prime factor in N!
--by summing repeated divisions of N by p
--My intuition is that there is a closed-form solution to this that would save considerable time.
--failing that, dynamic programming would help.
rdiv n p = g p 0 where
  g x s | x > n = s
        | otherwise =  g (x*p) (s+ n `div` x)

--Tree merging with wheel sieve.
primes = [2,3,5,7] ++ _Y ((11:) . tail . gapsW 11 wheel . joinT . hitsW 11 wheel)

--a Y combinator.
_Y g = g (_Y g)

joinT ((x:xs):t) = x : (union xs . joinT . pairs) t where
  pairs (xs:ys:t) = union xs ys : pairs t

gapsW k (d:w) s@(c:cs) | k < c = k : gapsW (k+d) w s
                       | otherwise = gapsW (k+d) w cs
hitsW k (d:w) s@(p:ps) | k < p = hitsW (k+d) w s
                       | otherwise = scanl (\c d -> c + p*d) (p*p) (d:w) : hitsW (k+d) w ps
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

union (x:xs) (y:ys) = case (compare x y) of
  LT -> x:union xs (y:ys)
  EQ -> x:union xs ys
  GT -> y:union (x:xs) ys
union xs [] = xs
union [] ys = ys
