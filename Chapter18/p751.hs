-- Short Exercise: Either Monad
-- Implement the Either Monad.

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure a = Second a
  First a <*> _ = First a
  _       <*> First b = First b
  Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b
