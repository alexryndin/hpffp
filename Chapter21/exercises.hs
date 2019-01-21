module Exercises21 where

import Chapter17.P686
import Chapter20.P804
import Chapter17.Exercises
import Chapter18.Exercises
import Chapter16.P646
import Chapter15.Exercises hiding (Identity)
import Chapter20.Exercises hiding (Constant)


import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- 2.
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant a <*> Constant b = Constant $ a <> b

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance Eq a => EqProp (Constant a b) where (=-=) = eq

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

-- 3.

instance Functor Optional where
  fmap f (Yep a) = Yep (f a)
  fmap _ Nada = Nada

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  Yep f <*> Yep a = Yep (f a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Yep a, return Nada]

instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Traversable Optional where
  traverse f (Yep a) = Yep <$> f a
  traverse f (Nada) = pure $ Nada

-- 4.

instance Foldable List where
  foldMap f (Cons x xs) = f x <> foldMap f xs
  foldMap f (Nil) = mempty

instance Traversable List where
  --traverse f = foldr (\a b -> (fmap (flip Cons Nil) (f a)) <> b) mempty
  sequenceA Nil = pure Nil
  sequenceA xa  = foldr (\a b -> a <*> b) (pure Nil) $ (fmap . fmap) Cons xa
  -- Inspired by Internet
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  traverse _ Nil = pure Nil

-- 5.

instance Traversable (Three a b) where
  sequenceA (Three a b c) = Three a b <$> c

-- 6.
instance Traversable (Three' a) where
  sequenceA (Three' a b c) = Three' a <$> b <*> c

-- 7.
data S n a = S (n a) a deriving (Show, Eq)

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ S a b

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S a b) = S (fmap f a) $ f b

instance Applicative (S n)

main = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger)
  let triggerr = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable triggerr)
  let triger3 = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable triger3)
  let trugger4 = undefined :: List (Int, Int, [Bool])
  quickBatch (traversable trugger4)
  let tringer5 = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable tringer5)
  let trigger6 = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
  quickBatch (traversable trigger6)
