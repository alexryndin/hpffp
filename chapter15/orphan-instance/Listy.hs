module Listy where

newtype Listy a = Listy [a] deriving (Eq, Show)

--instance Semigroup (Listy a) where
--  (Listy l) <> (Listy l') = Listy $ l <> l'
--instance Monoid (Listy a) where
--  mempty = Listy []
