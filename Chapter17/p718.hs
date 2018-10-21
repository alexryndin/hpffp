-- Validation has the same representation as Either, but it can be dif-
-- ferent. The Functor will behave the same, but the Applicative will be
-- different. See above for an idea of how Validation should behave.
-- Use the checkers library.

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)
-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a
-- This is different
instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  Failure e1 <*> Failure e2 = Failure $ e1 <> e2
  Failure e1 <*> _          = Failure e1
  _          <*> Failure e2 = Failure e2
  Success f  <*> Success a  = Success $ f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure a, Success b]


instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (Success ("a", 1, 11) :: Validation String (String, Sum Int, Sum Double))
  quickBatch $ applicative (Failure "a"          :: Validation String (String, Sum Int, Sum Double))
