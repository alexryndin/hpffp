-- Write applicative instances for the following datatypes. Confused?
-- Write out what the type should be. Use the checkers library to validate
-- the instances.

module Chapter17.Exercises where

import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Monoid
import Chapter16.P646
import Chapter15.Exercises
import Control.Applicative (liftA3)

-- 1.

instance Applicative Pair where
  pure a = Pair a a
  Pair f1 f2 <*> Pair v1 v2 = Pair (f1 v1) (f2 v2)

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

-- 2.

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  Two a1 f <*> Two a2 v = Two (a1 <> a2) (f v)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- 3.

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  Three a1 a2 f <*> Three b1 b2 v = Three (a1 <> b1) (a2 <> b2) (f v)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- 4.

instance (Monoid a) => Applicative (Three' a) where
  pure a = Three' mempty a a
  Three' a f1 f2 <*> Three' b v1 v2 = Three' (a <> b) (f1 v1) (f2 v2)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- 5.

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  Four a1 a2 a3 f <*> Four b1 b2 b3 v = Four (a1 <> b1) (a2 <> b2) (a3 <> b3) (f v)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- 6.

instance (Monoid a) => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  Four' a1 a2 a3 f <*> Four' b1 b2 b3 v = Four' (a1 <> b1) (a2 <> b2) (a3 <> b3) (f v)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Pair (String, Sum Int, Sum Double))
  quickBatch $ applicative (undefined :: Two String (String, Sum Int, Sum Double))
  quickBatch $ applicative (undefined :: Three String String (String, Sum Int, Sum Double))
  quickBatch $ applicative (undefined :: Three String String (String, [Int], Sum Double))
  quickBatch $ applicative (undefined :: Three' String (String, Sum Int, Sum Double))
  quickBatch $ applicative (undefined :: Four String String String (String, [Int], Sum Double))
  quickBatch $ applicative (undefined :: Four' String (String, [Int], Sum Double))
  putStrLn $ show $ combos stops vowels stops
