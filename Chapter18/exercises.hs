import Prelude hiding (Right, Left, Identity)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- 1.
data Nope a =
  NopeDotJpg deriving (Show, Eq)

instance Eq a => EqProp (Nope a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = do
    return NopeDotJpg

instance Functor Nope where
  fmap _ (NopeDotJpg) = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

-- 2.
data PEither b a =
  Left a | Right b deriving (Eq, Show)

instance Functor (PEither b) where
  fmap f (Left a) = Left $ f a
  fmap _ (Right a) = Right a

instance Applicative (PEither b) where
  pure x = Left x
  Right a <*> _ = Right a
  _      <*> Right b = Right b
  Left a <*> Left b = Left $ a b

instance Monad (PEither b) where
  Right a >>= _ = Right a
  Left a >>= b = b a

instance (Eq a, Eq b) => EqProp (PEither a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Left a,
           return $ Right b]

-- 3.
newtype Id a = Id a
  deriving (Eq, Ord, Show)

instance Functor Id where
  fmap f (Id a) = Id $ f a

instance Applicative Id where
  pure a = Id a
  Id f <*> Id a = Id $ f a

instance Monad Id where
  Id a >>= f = f a

instance Eq a => EqProp (Id a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (Id a) where
  arbitrary = do
    a <- arbitrary
    return $ Id a

-- 4.
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

instance Monad List where
  Nil >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]

main = do
  let trigger = undefined :: Nope (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  let triggerr = undefined :: PEither (Int, String, Int) (Int, String, Int)
  quickBatch $ functor triggerr
  quickBatch $ applicative triggerr
  quickBatch $ monad triggerr
  let triggerrr = undefined :: Id (Int, String, Int)
  quickBatch $ functor triggerrr
  quickBatch $ applicative triggerrr
  quickBatch $ monad triggerrr
  let triggerrrr = undefined :: List (Int, String, Char)
  quickBatch $ functor triggerrrr
  quickBatch $ applicative triggerrrr
  quickBatch $ monad triggerrrr



-- Write the following functions using the methods provided by
-- Monad and Functor. Using stuff like identity and composition is fine,
-- but it has to typecheck with types provided.

-- 1. j :: Monad m => m (m a) -> m a
j :: Monad m => m (m a) -> m a
j m = m >>= id

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = fmap f m

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a m mf = mf <*> m

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs k = foldr h (return []) $ fmap k xs
  where h x m = do
          x' <- x
          m' <- m
          return (x':m')

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id
