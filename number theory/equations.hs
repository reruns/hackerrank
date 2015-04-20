--This is too slow. Probably should just precompute the primes.
--OTOH it would probably be more than 2M of text and Atom would complain.
--primes = filter isPrime [2..1000000]
solve x = foldl (\z y -> z*(1+2*rdiv x y) `mod` (10^6+7)) 1 (takeWhile (<=x) primes)

--rdiv calculates the multiplicity of a prime factor in N!
--by summing repeated divisions of N by p
--My intuition is that there is a closed-form solution to this that would save considerable time.
--failing that, dynamic programming would help.
rdiv n p = g (div n p) 0 where
  g 0 s = s
  g x s = g (div x p) (s+x)


primes = 2:3:minus [5,7..] (foldr (\p r -> p*p : union [p*p+2*p, p*p+4*p..] r) [] (tail primes))

minus (x:xs) (y:ys) = case (compare x y) of
  LT -> x : minus xs (y:ys)
  EQ ->     minus xs ys
  GT ->     minus (x:xs) ys
minus xs _ = xs

union (x:xs) (y:ys) = case (compare x y) of
  LT -> x:union xs (y:ys)
  EQ -> x:union xs ys
  GT -> x:union (x:xs) ys
union xs [] = xs
union [] ys = ys
