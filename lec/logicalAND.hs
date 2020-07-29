logicalAND x y  | x == True = y
                | y == True = x
                | y == False = False