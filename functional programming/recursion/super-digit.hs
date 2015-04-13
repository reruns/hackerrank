droot x = if x < 10 then x else droot $ (mod x 10) + (div x 10)
super n k = droot $ (droot n) * k
