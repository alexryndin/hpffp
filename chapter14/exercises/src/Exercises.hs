module Exercises where

import Data.List
import Data.Char
import Test.QuickCheck


digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = error "fuck you"

digits :: Int -> [Int]
digits n = let
  h n' | div n' 10 == 0 = n' : []
      | otherwise = (mod n' 10) : h (div n' 10) 
  in reverse $ h n

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

half :: Double -> Double
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x


multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y =
  x * y == y * x

quotLaw :: Integral a => a -> a -> Bool
quotLaw x y = y == 0 || (quot x y)*y + (rem x y) == x

divLaw :: Integral a => a -> a -> Bool
divLaw x y  = y == 0 || (div x y)*y + (mod x y) == x


powAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
powAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

powCommutative :: (Eq a, Integral a) => a -> a -> Bool
powCommutative x y =
  x ^ y == y ^ x

listReverse :: (Eq a) => [a] -> Bool
listReverse x = id x == reverse (reverse x)

propDollar :: (Eq a, Num a) => a -> Bool
propDollar x = ((-) x $ (x+x)) == (-) x (x+x)

propCons :: Eq a => [a] -> [a] -> Bool
propCons xs ys = foldr (:) xs ys == (++) xs ys

propConcat :: (Eq a, Foldable t) => t [a] -> Bool
propConcat xs = foldr (++) [] xs == concat xs


propex10 :: Int -> [a] -> Bool
propex10 n xs = length (take n xs) == n

propRead :: (Eq a, Read a, Show a) => a -> Bool
propRead x = (read (show x)) == x

square :: Num a => a -> a
square x = x * x

squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x = (square . sqrt) x == x

twice f = f . f
fourTimes = twice . twice


capitalizeWord :: String -> String
capitalizeWord = map toUpper

f1 :: String -> Bool
f1 x = twice capitalizeWord x == fourTimes capitalizeWord x

f2 :: [Int] -> Bool
f2 x = twice sort x == fourTimes sort x

data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = oneof [return Fulse,
                     return Frue]

getFool75 :: Gen Fool
getFool75 = frequency [ (1, return Fulse)
                      , (3, return Frue)]
