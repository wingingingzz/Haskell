logicalOR False False = False
logicalOR _ _ = True


logicalOR' True _ = True
logicalOR' _ True = True
logicalOR' _ _ = False

logicalOR'' False b = b
logicalOR'' _ False = True
logicalOR'' _ _ = True