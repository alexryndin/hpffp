-- Implement Functor instances for the following datatypes. Use the
-- QuickCheck properties we just showed you to validate them.
module Chapter16.P646 where

import Test.QuickCheck
import Test.QuickCheck.Function
import Chapter15.Exercises

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool

functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type InttoInt = Fun Int Int
type StringtoInt = Fun String Int

-- 1.

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

type IdentityFunctor = Identity Int -> InttoInt -> InttoInt -> Bool

-- 2.

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

type PairFunctor = Pair Int -> InttoInt -> InttoInt -> Bool
type PairFunctor' = Pair String -> StringtoInt -> InttoInt -> Bool

-- 3.

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

type TwoFunctor = Two Int Int -> InttoInt -> InttoInt -> Bool
type TwoFunctor' = Two String String -> StringtoInt -> InttoInt -> Bool

-- 4.

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

type ThreeFunctor = Three Int Int Int -> InttoInt -> InttoInt -> Bool
type ThreeFunctor' = Three String String String -> StringtoInt -> InttoInt -> Bool

-- 5.
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

type ThreeFunctor'' = Three' Int Int -> InttoInt -> InttoInt -> Bool
type ThreeFunctor''' = Three' String String -> StringtoInt -> InttoInt -> Bool
-- 6.

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d


type FourFunctor = Four Int Int Int Int -> InttoInt -> InttoInt -> Bool
type FourFunctor' = Four String String String String -> StringtoInt -> InttoInt -> Bool


-- 7.

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c $ f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

type FourFunctor'' = Four' Int Int -> InttoInt -> InttoInt -> Bool
type FourFunctor''' = Four' String String -> StringtoInt -> InttoInt -> Bool

main :: IO ()
main = do
  quickCheck (functorCompose' :: IdentityFunctor)
  quickCheck (functorCompose' :: PairFunctor)
  quickCheck (functorCompose' :: PairFunctor')
  quickCheck (functorCompose' :: TwoFunctor')
  quickCheck (functorCompose' :: TwoFunctor)
  quickCheck (functorCompose' :: ThreeFunctor)
  quickCheck (functorCompose' :: ThreeFunctor')
  quickCheck (functorCompose' :: ThreeFunctor'')
  quickCheck (functorCompose' :: ThreeFunctor''')
  quickCheck (functorCompose' :: FourFunctor)
  quickCheck (functorCompose' :: FourFunctor')
  quickCheck (functorCompose' :: FourFunctor'')
  quickCheck (functorCompose' :: FourFunctor''')
