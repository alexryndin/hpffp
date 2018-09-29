import Data.List (intersperse)

  
sumr :: (Eq a, Num a) => a -> a
sumr n  | signum n == -1 = 0
        | otherwise = let
            go n s | n == 0 =  s
                   | signum n == -1 = 0
                   | otherwise = go (n - 1) (s + n)
            in go n 0

rmult :: (Integral a) => a -> a -> a
rmult 0 _ = 0
rmult _ 0 = 0
rmult a b | a < 0 && b < 0 =   rmult (-a) (-b)
          | a < 0          = - rmult (-a)   b
          | b < 0          = - rmult   a  (-b)
          | otherwise = let
              go 0 b = 0
              go a b = b + go (a-1) b
              in go a b

data DividedResult a = Result (a, a) | DividedByZero deriving Show

-- This function works like divMod
dividedBy :: Integral a => a -> a -> DividedResult a   
dividedBy num 0 = DividedByZero
dividedBy a b | a < 0  && b < 0 = Result $ ( a1, -b1)
              | a < 0  && b > 0 = Result $ (-a2,  b2)
              | a >= 0 && b < 0 = Result $ ( a1, -b1)
              | a >= 0 && b > 0 = Result $ ( a1,  b1)
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
        go2 n d count
          | n >= 0 = (count, n)
          | otherwise = go2 (n + d) d (count + 1)
        (a1, b1) = go (abs a) (abs b) 0
        (a2, b2) = go2 (a) (abs b) 0


mc91 x | x > 100 = x - 10
       | otherwise = mc91 $ mc91 $ (x+11)


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
  h n | div n 10 == 0 = n : []
      | otherwise = (mod n 10) : h (div n 10) 
  in reverse $ h n

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
