{-# LANGUAGE NoMonomorphismRestriction #-}

e11 = (* 9) 6                                    -- Num a => a
e12 = head [(0,"doge"),(1,"kitteh")]             -- Num a => (a, [Char])
e13 = head [(0 :: Integer ,"doge"),(1,"kitteh")] -- (Integer, [Char])
e14 = if False then True else False              -- Bool
e15 = length [1, 2, 3, 4, 5]                     -- Int
e16 = (length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool


bigNum = (^) 5
wahoo = bigNum $ 10

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ (xToY x)))

i :: a -> a
i a = a

c :: a -> b -> a
c i j = i

c'' :: b -> a -> b
c'' i j = i

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r x = tail x

co :: (b -> c) -> (a -> b) -> a -> c
co f g a = f $ g a

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a -> b) -> a -> b
a' f a = f a
