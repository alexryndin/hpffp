import Data.List

stops = "pbtdkg"
vowels = "aeiou"

f1a = [(a,b,c) | a <- stops, b <- vowels, c <- stops]
f2a = filter (\(a,_,_) -> a == 'p') f1a
f2a' = [(a,b,c) | a <- stops, b <- vowels, c <- stops, a == 'p']

seekritFunc :: String -> Double
seekritFunc x = (/) (fromIntegral (sum (map length (words x))))
                    (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (==) e) False
myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr h []
  where h x l = case f x of
                  True  -> x:l
                  False -> l

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl' h x xs where
  h a b = case f b a of
        GT -> b
        _  -> a

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)
myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f (x:xs) = foldl' h x xs where
  h a b = case f a b of
        GT -> b
        _  -> a
