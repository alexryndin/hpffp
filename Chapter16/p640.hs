-- Add fmap, parentheses, and function composition to the expression
-- as needed for the expression to typecheck and produce the expected
-- result. It may not always need to go in the same place, so don’t get
-- complacent.

-- 1.

a = fmap (+1) $ read "[1]" :: [Int]

-- 2.

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.

c = (*2) . (\x -> x - 2)
c' = fmap (*2) (\x -> x - 2)

-- 4.

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
d' = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123"++) $ (fmap show ioi)
    in fmap (*3) changed

