pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
