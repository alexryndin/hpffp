f1 :: [Integer]
f1 = filter (\x -> (rem x 3) == 0) [1..30]
f1' :: [Integer]
f1' = [x | x <- [1..30], rem x 3 == 0]

f2 :: Int
f2 = length $ f1

f3 s = filter (\x -> notElem x ["A", "a", "the", "The", "An", "a"]) $ words s
