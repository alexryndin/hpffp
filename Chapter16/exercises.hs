{-# LANGUAGE FlexibleInstances #-}
-- Determine if a valid Functor can be written for the datatype provided.

-- 1.
-- data Bool = False | True - no way

-- 2.

data BoolAndSomethingElse a =
  False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' $ f a
  fmap f (True' a)  = False' $ f a

-- 3. data BoolAndMaybeSomethingElse a = -- Same as for maybe
--      Falsish | Truish a

-- 4. newtype Mu f = InF { outF :: f (Mu f) } -- no bacuse of kind (* -> *) -> *

-- 5.
-- No, because of * kind
-- data D = D (Array Word Word) Int Int

-- Rearrange the arguments to the type constructor of the datatype
-- so the Functor instance works.

-- 1.
data Sum b a = First a
             | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a c b = DeepBlue a c
                   | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1.

data Quant a b = Finance
               | Desk a
               | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

-- 2.

data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

-- 3.

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a
  deriving (Show, Eq)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' $ f a

-- 4.

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.

data LiftItOut f a =
  LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut $ fmap f g

-- 6.

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-- 7.

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

-- 8.

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious a b c) = Notorious a b $ fmap f c

-- 9.

data List a = Nil
            | Cons a (List a)
            deriving Show

instance Functor List where
  fmap f (Nil)    = Nil
  fmap f (Cons a l) = Cons (f a) $ fmap f l

-- 10.

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving Show

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11.

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read f')     = Read $ fmap f f'
