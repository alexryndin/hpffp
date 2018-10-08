{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

newtype Herd = Herd (Int, String)
newtype Herd2 = Herd2 (Int, Int)
--newtype (Num a, TooMany a) => Herd3 = Herd3 (a, a)

instance TooMany Herd where
  tooMany (Herd (n, _)) = n > 42

instance TooMany Herd2 where
  tooMany (Herd2 (a, b)) = (a+b) > 42

-- Requires DatatypeContexts Pragma
--instance (Num a, TooMany a) => Herd3 where
--  tooMany (Herd3 (a, b)) = tooMany a

--instance TooMany (Int, String) where
--  tooMany (n, _) = n > 42
--
--instance TooMany (Int, Int) where
--  tooMany (x, y) = (x+y) > 42
--
--
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x 
