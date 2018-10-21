-- Implement the List Applicative. Writing a minimally complete Ap-
-- plicative instance calls for writing the definitions of both pure and
-- <*>. We’re going to provide a hint as well. Use the checkers library to
-- validate your Applicative instance.

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative


data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a                     = Cons a Nil
  Nil        <*> _           = Nil
  _          <*> Nil         = Nil
  (Cons f fs) <*> vs = append (fmap f vs) (fs <*> vs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]

instance Semigroup a => Semigroup (ZipList' a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty



-- Implement the ZipList Applicative. Use the checkers library to vali-
-- date your Applicative instance. We’re going to provide the EqProp
-- instance and explain the weirdness in a moment.

--newtype ZipList' a =
--  ZipList' (List a)
--  deriving (Eq, Show)
--
--instance Functor ZipList' where
--  fmap f (ZipList' xs) = ZipList' $ fmap f xs
--
--instance Applicative ZipList' where
--  pure a                                        = ZipList' $ Cons a Nil
--  ZipList' Nil         <*> _                    = ZipList' Nil
--  _                    <*> ZipList' Nil         = ZipList' Nil
--  ZipList' (Cons f fs) <*> ZipList' (Cons v vs) = ZipList' $ Cons (f v) (fs <*> vs)

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) (xs)

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a                                        = ZipList' $ repeat a
  ZipList' fs <*> ZipList' vs = ZipList' $ l fs vs
    where l [] _ = []
          l _ [] = []
          l (f:fs) (v:vs) = (f v) : l fs vs

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
            in take' 3000 l
          ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

main :: IO ()
main = do
  quickBatch (applicative $ Cons (([1], "a", 'c') :: ([Int], String, Char)) Nil)
  quickBatch (applicative $ Cons ((1 , "a", 'c') :: (Int, String, Char)) Nil)
  quickBatch (applicative $ ZipList' [(1 , "a", 'c') :: (Int, String, Char)])
