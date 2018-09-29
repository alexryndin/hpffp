myWords :: String -> [String]
myWords []  = []
myWords (' ':s) = myWords s
myWords s = (takeWhile (/= ' ') s) : myWords (dropWhile (/= ' ') s)
