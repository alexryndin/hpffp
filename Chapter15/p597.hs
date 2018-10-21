-- Write a Monoid instance for Maybe type which doesn’t require a Monoid
--for the contents. Reuse the Monoid law QuickCheck properties and
--use them to validate the instance.

import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return $ First' (Only a))
              , (1, return $ First' (Nada  )) ]

instance Semigroup (First' a) where
  First' Nada <> First' Nada = First' (Nada)
  First' Nada <> First' b    = First' b
  First' a    <> First' Nada = First' a
  First' a    <> First' b    = First' a

instance Monoid (First' a) where
  mempty = First' Nada


type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
