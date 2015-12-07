grid r c = b + 2 * (c-1) + beven r where
           b = 10 * (div (r - 1) 2)
           
beven x = if even x then 1 else 0

main = do
    s <- getLine
    let [r,c] = map read $ words s
    print $ grid r c