fibs = 0:1: zipWith (+) fibs (tail fibs)

fib n = head $ drop (n-1) fibs
