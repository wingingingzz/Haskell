oneOfTriple :: (Int,Int,String) -> Int
oneOfTriple (x,_,_) = x

oneOfTriple' :: (Int,Int,String) -> String
oneOfTriple' (_,_,x) = x

oneOfTriple'' :: (a,b,c) -> c
oneOfTriple'' (_,_,x) = x

howMany :: Eq a => a -> [a] -> Int
howMany y [] = 0
howMany y (x:xs)    | x==y = 1 + howMany y xs
                    | otherwise = howMany y xs

sumLess :: Int -> [Int] -> Int
sumLess y [] = 0
sumLess y (x:xs)    | x<y = x + sumLess y xs
                    | otherwise = sumLess y xs