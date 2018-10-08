import Data.Char
import Data.List

vigenere :: String -> String -> String
vigenere w msg = let
  h n' c = if (c>'A')
           then chr ((charn c + n') `mod` 26 + shift c)
           else c
  in zipWith h keyl msg
  where shift c = case isUpper c of
          True  -> ord 'A'
          False -> ord 'a'
        charn c = ord c - shift c
        --n' = n `mod` 26
        key = map ((flip (-)) (ord 'A') . (ord . toUpper)) w
        keyl = take (length msg) (cycle key)

unvigenere :: String -> String -> String
unvigenere w msg = let
  h n' c = if (c>'A')
           then chr ((charn c + (26-n')) `mod` 26 + shift c)
           else c
  in zipWith h keyl msg
  where shift c = case isUpper c of
          True  -> ord 'A'
          False -> ord 'a'
        charn c = ord c - shift c
        --n' = n `mod` 26
        key = map ((flip (-)) (ord 'A') . (ord . toUpper)) w
        keyl = take (length msg) (cycle key)
--uncaesar n = caesar (26 - n `mod` 26)


isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' []    _                    = True
isSubsequenceOf' _     []                   = False
isSubsequenceOf' a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\xs@(x:xs') -> (xs, (toUpper x):xs')) . words

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x):xs

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate " " . cap1 . words
  where h :: [String] -> [String]
        h [] = []
        h (x1:[]) = [x1]
        h (x1:x2:xs) = if  (x1 !! (length x1 - 1)) == '.'
                       then x1 : h (capitalizeWord x2:xs)
                       else x1 : h (x2:xs)
        cap1 [] = []
        cap1 (x:xs) = h $ capitalizeWord x : xs

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2)= eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
