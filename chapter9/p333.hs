myZip :: [a] -> [b] -> [(a, b)]
myZip [] _  = []
myZip _  [] = []
myZip (a:ax) (b:bx) = (a, b) : myZip ax bx

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _  []         = []
myZipWith f (a:ax) (b:bx) = (f a b) : myZipWith f ax bx

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
