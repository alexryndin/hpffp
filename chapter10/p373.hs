fibs = 1 : scanl (+) 1 fibs


fibs1 = take 20 $ fibs

fibs2 = takeWhile (< 100) $ fibs
-- fibs2' = [x | x <- fibs, x < 100]
  
fac = scanl (*) 1 [1..]
