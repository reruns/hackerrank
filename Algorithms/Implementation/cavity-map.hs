-- cavs (a:b:c:xs) = let ch = zip3 a b c in
--     (fix (\f (x:xs) y -> if xs == [] then [show x] else
--                          if snd x > fst x &&
--                             snd x > ))
-- fst and snd aren't defined for tuples of size >2
