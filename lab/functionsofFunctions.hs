add :: Int -> Int -> Int
add x y = x+y

doAll :: (Int -> Int -> Int) -> [Int] -> Int
doAll f [x] = x
doAll f (x:xs) = f x (doAll f xs)

transIfSumGT :: Int -> ([Int] -> [Int]) -> [Int] -> [Int]
transIfSumGT n f (x:xs) | doAll add (x:xs) > n = f (x:xs)
                        | otherwise = (x:xs)

