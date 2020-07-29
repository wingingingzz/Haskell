concat' [] = []
concat' (xs : xss) = xs ++ concat' xss