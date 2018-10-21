-- fixed
data FixMePls a = FixMe | Pls deriving (Eq, Show)
instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap _ Pls = Pls
