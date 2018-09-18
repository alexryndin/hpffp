f1 :: String -> String
f1 a = a ++ "!"

f2 :: String -> String
f2 a = [a !! 4]

f3 :: String -> String
f3 a = drop 9 a

thirdLetter :: String -> Char
thirdLetter x = x !! 2

rvrs :: String -> String
rvrs s = drop 9 s ++ (take 4 $ drop 5 s)  ++ take 5 s
