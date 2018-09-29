-- p235
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a,b,c) (d,e,f) = ((a,d), (c, f))

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst $ x `divMod` 10
        d = snd $ xLast `divMod` 10

hndsDigit :: Integral a => a -> a
hndsDigit x = d
  where xLast = fst $ x `divMod` 100
        d = snd $ xLast `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool a b t = case t of
                   True -> a
                   False -> b

foldBool' :: a -> a -> Bool -> a
foldBool' a b t
  | t == True = a
  | otherwise = b

g :: (a -> b) -> (a, c) -> (b, c)
g f' (a, b) = (f a, b)
