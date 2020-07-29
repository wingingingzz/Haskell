takeWhile'::(a->Bool)->[a]->[a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x:takeWhile' p xs
                    | otherwise = []

getLine'::IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else 
                 do xs <- getLine'
                    return (x:xs)

putStr'::String->IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn'::String->IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen'::IO()
strlen' = do putStr' "Please enter a string: "
             xs <- getLine'
             putStr' "The length is: "
             putStrLn' (show (length xs))

e1 = \x -> (x+)

e2::[Int]
e2 = [1,2,3]

e3::a->b->(a,b,a,b)
e3 x y = (x,y,x,y)

e4::[a]->[a]->[a]
e4 xs ys = xs ++ ys

e5::(Bool,Bool,Bool)
e5 = (True, False, True)

data Expr = Val Int | Sub Expr Expr

eval::Expr->Int
eval (Val n) = n
eval (Sub x y) = eval x - eval y

subs::Expr->Int
subs (Val n) = 0
subs (Sub x y) = 1 + subs x + subs y

data KVStore k v = KVPair k v (KVStore k v) | Empty