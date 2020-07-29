triangle :: Int -> Int
triangle 0 = 0
triangle n = n + triangle (n-1)

triangle' :: Int -> Int
triangle' 0 = 0
triangle' n = n^2 + triangle' (n-1)