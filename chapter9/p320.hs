import Prelude hiding (length)
length :: [a] -> Integer
length [] = 0
length (_:xs) = 1 + length xs
