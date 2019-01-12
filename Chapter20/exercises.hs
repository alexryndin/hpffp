module Exercises20 where

import Data.Foldable
import Data.Monoid
import Chapter16.P646
import Chapter15.Exercises


mySum :: (Foldable t, Num a) => t a -> a
mySum t = getSum $ foldMap Sum t

myProduct :: (Foldable t, Num a) => t a -> a
myProduct t = getProduct $ foldMap Product t

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem a t = foldr (\b flag -> flag || b == a) False t


newtype Max a = Max {getMax :: Maybe a} deriving (Eq, Show)
newtype Min a = Min {getMin :: Maybe a} deriving (Eq, Show)

instance Ord a => Semigroup (Min a) where
  Min Nothing <> x = x
  x <> Min Nothing = x
  (Min a) <> (Min a') = Min (min a a')

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing


instance Ord a => Semigroup (Max a) where
  Max Nothing <> x = x
  x <> Max Nothing = x
  (Max a) <> (Max a') = Max (max a a')

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum t = getMin $ foldMap (Min . Just) t

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum t = getMax $ foldMap (Max . Just) t

myNull :: (Foldable t) => t a -> Bool
myNull = foldr (\_ _ -> False) True

myLength :: (Foldable t) => t a -> Int
myLength t = foldr (\_ c -> c + 1) 0 t

myToList :: (Foldable t) => t a -> [a]
myToList t = foldr (:) [] t

myFold :: (Foldable t, Monoid m) => t m -> m
myFold t = foldMap (id) t

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f t = foldr ((<>) . f) mempty t


-- Write Foldable instances for the following datatypes.
-- 1.
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr _ b (Constant a) = b

-- 2.
instance Foldable (Two a) where
  foldr f b (Two _ a) = f a b

-- 3.
instance Foldable (Three a b) where
  foldr f b (Three _ _ a) = f a b

-- 4.
instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = (f b) <> f c
  --foldr f b (Three' _ a c) = c `f` (a `f` b)

-- 5.
data Four'' a b = Four'' a b b b
instance Foldable (Four'' a) where
  --foldr f acc (Four'' _ a b c) = c `f` (b `f` (a `f` acc))
  foldMap f (Four'' _ a b c) = f a <> f b <> f c

-- Thinking cap time. Write a filter function for Foldable types using
-- foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
