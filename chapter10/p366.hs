import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = let
  f (DbDate x) b = x : b
  f _ b = b
  in foldr f [] db

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = let
  f (DbNumber x) b = x : b
  f _ b = b
  in foldr f [] db


-- Possibly faster (?)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = let
  f (DbDate x) (b) = case x > b of
                              True  -> x
                              False -> b
  f _ b = b
  fstDate [] = error "Empry db or db doesn't contain any date"
  fstDate ((DbDate x): xs) = x
  fdtDate (_:xs) = fstDate xs
  in foldr f (fstDate db) db

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' db = maximum . filterDbDate $ db


sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

sumDb' :: [DatabaseItem] -> Integer
sumDb' db = let
  f (DbNumber a) b = a + b
  f _ b = b
  in foldr f 0 db

sumDb'' :: [DatabaseItem] -> Integer
sumDb'' db = foldr (\a b -> a + b) 0 . filterDbNumber $ db

avgDb :: [DatabaseItem] -> Double
avgDb db =  let
  list = filterDbNumber db
  l   = fromIntegral $ length list
  sum = fromIntegral $ sumDb db
  in if l == 0 then 0 else sum / l


avgDb' :: [DatabaseItem] -> Double
avgDb' db = let
  f (DbNumber a) (t, c) = (t+a, c+1)
  f _ t = t
  (t, c) = foldr f (0, 0) db
  in if c == 0 then 0 else fromIntegral t / c
