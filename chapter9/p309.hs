myWords :: Char -> String -> [String]
myWords _ []  = []
myWords a (a':s) | a == a' = myWords a s
myWords a s = (takeWhile (/= a) s) : myWords a (dropWhile (/= a) s)
