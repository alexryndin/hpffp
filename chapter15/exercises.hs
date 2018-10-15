-- Given a datatype, implement the Semigroup instance. Add Semi-
-- group constraints to type variables where needed. Use the Semigroup
-- class from the semigroups library or write your own. When we use
-- <>, we mean the infix mappend from the Semigroup typeclass.

import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  Identity a <> Identity b = Identity $ a <> b

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a1 b1 c1 <> Three a2 b2 c2 = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc =  Three String (Sum Int) (Product Int)
                -> Three String (Sum Int) (Product Int)
                -> Three String (Sum Int) (Product Int)
                -> Bool

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a1 b1 c1 d1 <> Four a2 b2 c2 d2 = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc =   Four String (Sum Int) (Product Int) [Integer]
                -> Four String (Sum Int) (Product Int) [Integer]
                -> Four String (Sum Int) (Product Int) [Integer]
                -> Bool

-- 6.
newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  BoolConj False <> BoolConj _     = BoolConj False
  BoolConj _     <> BoolConj False = BoolConj False
  BoolConj True  <> BoolConj True  = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = frequency [( 1, return $ BoolConj True)
                        ,( 1, return $ BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
  BoolDisj True   <> BoolDisj _     = BoolDisj True
  BoolDisj _      <> BoolDisj True  = BoolDisj True
  BoolDisj False  <> BoolDisj False = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [( 1, return $ BoolDisj True)
                        ,( 1, return $ BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _     = Snd a
  _     <> Snd b = Snd b
  a     <> _     = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a)
              ,(1, return $ Snd b)]

type OrAssoc = Or Int String -> Or Int String -> Or Int String -> Bool

-- 9.
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

-- No tests

-- 10.
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp a <> Comp b = Comp (a . b)

-- No tests

-- 11.

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup (Validation a b) where
  Success a <> _         = Success a
  _         <> Success b = Success b
  _         <> a         = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure a)
              ,(1, return $ Success b)]

type ValidationAssoc = Validation Int String -> Validation Int String -> Validation Int String -> Bool

-- 12.

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Failure a) <> AccumulateRight (Failure b) = AccumulateRight (Failure b)
  AccumulateRight (Success a) <> AccumulateRight (Failure b) = AccumulateRight (Success a)
  AccumulateRight (Failure a) <> AccumulateRight (Success b) = AccumulateRight (Success b)
  AccumulateRight (Success a) <> AccumulateRight (Success b) = AccumulateRight (Success (a <> b))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ AccumulateRight (Failure a))
              ,(1, return $ AccumulateRight (Success b))]

type AccumRightAssoc = AccumulateRight Int String -> AccumulateRight Int String -> AccumulateRight Int String -> Bool

-- 13.

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure a) <> AccumulateBoth (Failure b) = AccumulateBoth (Failure (a <> b))
  AccumulateBoth (Success a) <> AccumulateBoth (Failure b) = AccumulateBoth (Success a)
  AccumulateBoth (Failure a) <> AccumulateBoth (Success b) = AccumulateBoth (Success b)
  AccumulateBoth (Success a) <> AccumulateBoth (Success b) = AccumulateBoth (Success (a <> b))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ AccumulateBoth (Failure a))
              ,(1, return $ AccumulateBoth (Success b))]

type AccumBothAssoc = AccumulateBoth (Sum Int) String -> AccumulateBoth (Sum Int) String -> AccumulateBoth (Sum Int) String -> Bool


-- Given a datatype, implement the Monoid instance. Add Monoid
-- constraints to type variables where needed. For the datatypes you’ve
-- already implemented Semigroup instances for, you just need to
-- figure out what the identity value is.

-- 1.

instance Monoid Trivial where
  mempty = Trivial

type TrivialLeftIdentity  = Trivial -> Bool
type TrivialRightIdentity = Trivial -> Bool

-- 2.

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity (mempty)

type IdentityLeftIdentity = Identity String -> Bool
type IdentityRightIdentity = Identity String -> Bool

-- 3.

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

type TwoLeftIdentity  = Two String (Sum Int) -> Bool
type TwoRightIdentity = Two String (Sum Int) -> Bool

-- 4.

instance Monoid BoolConj where
  mempty = BoolConj True

-- 5.

instance Monoid BoolDisj where
  mempty = BoolDisj False

-- 6.

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ (const mempty)

-- Note tests (yet hope)

-- 7.

instance Monoid (Comp a) where
  mempty = Comp id

-- 8.

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \x -> let
    (a1, s1) = f x in
    let
      (a2, s2) = g s1
    in (a1 <> a2, s2)

f' = Mem $ \s -> ("hi", s + 1)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

-- Note tests (yet hope)

-- main
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: TrivialLeftIdentity)
  quickCheck (monoidRightIdentity :: TrivialRightIdentity)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: IdentityLeftIdentity)
  quickCheck (monoidRightIdentity :: IdentityRightIdentity)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoLeftIdentity)
  quickCheck (monoidRightIdentity :: TwoRightIdentity)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumRightAssoc)
  quickCheck (semigroupAssoc :: AccumBothAssoc)
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
