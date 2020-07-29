factors n = [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], sum(factors x) - x == x]