import Data.Char

f2 :: String -> String
f2 s  = filter isUpper s


f3 :: String -> String
f3 [] = []
f3 (x:xs) = toUpper x : xs


f4 :: String -> String
f4 s = map toUpper s

f5 :: String -> Char
f5 [] = ' '
f5 s = toUpper . head $ s

-- no way to write f6 totally pointfree cuz head is not empty list safe.
-- f6 = undefined


caesar n s = let
  h n' c = chr ((charn c + n') `mod` 26 + shift c)
  in map (h $ n') s
  where shift c = case isUpper c of
          True  -> ord 'A'
          False -> ord 'a'
        charn c = ord c - shift c
        n' = n `mod` 26

uncaesar n = caesar (26 - n `mod` 26)


myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny (== x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (xs) = h xs []
  where h ([]) l = l
        h (x:xs) l = h xs $ x : l

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = h f xs x
  where h f [] c = c
        h f (x:xs) c = case f x c of
                         GT -> h f xs x
                         _  -> h f xs c

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = h f xs x
  where h f [] c = c
        h f (x:xs) c = case f x c of
                         LT -> h f xs x
                         _  -> h f xs c

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
