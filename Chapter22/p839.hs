import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM =
  cap >>= \a ->
  rev >>= \b ->
  return (a,b)

tupledMDO :: [Char] -> ([Char], [Char])
tupledMDO = do
  a <- cap
  b <- rev
  return (a,b)
