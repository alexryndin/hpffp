myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (a:_) = Just a
