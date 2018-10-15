-- Write the Monoid instance for our Maybe type renamed to Optional.
-- Note: https://stackoverflow.com/questions/52237895/could-not-deduce-semigroup-optional-a-arising-from-the-superclasses-of-an-in

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada     <> Nada     = Nada
  x        <> Nada     = x
  Nada     <> x        = x
  (Only x) <> (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
