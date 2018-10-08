import Data.List
import Data.Char

isVowel :: Char -> Bool
isVowel c = elem (toLower c) "aeiou"

isCons :: Char -> Bool
isCons c = elem (toLower c) "bcdfghjlmnpqrstvxzwy"

isThe :: String -> Bool
isThe s | s == "the" ||
          s == "The" = True
        | otherwise  = False

checkThe :: String -> Maybe String
checkThe s | s == "the" = Just "the"
           | s == "The" = Just "The"
           | otherwise  = Nothing

replaceThe :: String -> String
replaceThe = intercalate " " . map (h) . words
  where h s = case checkThe s of
                Just "the" -> "a"
                Just "The" -> "A"
                Nothing    ->  s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . foldl' h (0, False) . words
  where h (c, False) w      | isThe w   = (c  , True )
                            | otherwise = (c  , False)
        h (c, True)  (x:xs) | isVowel x = (c+1, False)
                            | otherwise = (c  , False)

countVowels :: String -> Integer
countVowels = foldr h 0
  where h char c = case isVowel char of
              True  -> c + 1
              False -> c

newtype Word' = Word' String deriving (Eq, Show)

validateWord :: String -> Maybe Word'
validateWord s = if v > c then Nothing else Just $ Word' s
  where (v, c) = foldr h (0, 0) s
        h char (v, c) | isVowel char = (v+1, c  )
                      | isCons  char = (v  , c+1)
                      | otherwise    = (v, c)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ i) = 1 + natToInteger i

integerToNat :: Integer -> Maybe Nat
integerToNat i | i < 0     = Nothing
               | otherwise = Just (iToNat i)
  where iToNat 0 = Zero
        iToNat n = Succ (iToNat $ n-1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee a f Nothing = a
mayybee a f (Just b) = f b

fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybee a id m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr h []
  where h Nothing xs  = xs
        h (Just x) xs = x:xs


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs | any isNothing xs = Nothing
             | otherwise = Just $ catMaybes xs

lefts' :: [Either a b] -> [a]
lefts' = foldr h []
  where h (Right _) xs = xs
        h (Left  x) xs = x:xs

rights' :: [Either a b] -> [b]
rights' = foldr h []
  where h (Left  _) xs = xs
        h (Right x ) xs = x:xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr h ([], [])
  where h (Left  l) (ls, rs) = (l:ls, rs)
        h (Right r) (ls, rs) = (ls, r:rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f b = either' (const Nothing) (Just . f) b

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x       = h $ f x
  where h Nothing       = []
        h (Just (a, b)) = a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = h $ f x
  where h Nothing          = Leaf
        h (Just (l, a, r)) = Node (unfold f l) a (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold h n
  where h n'| n'< 1     = Nothing
            | otherwise = Just (n'-1, n-n', n'-1)
