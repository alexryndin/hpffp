-- Write a Functor instance for a datatype identical to Maybe. We’ll use
-- our own datatype because Maybe already has a Functor instance and
-- we cannot make a duplicate one.

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap f LolNope     = LolNope
