module RegisteredUser where


data Username = Username String
data AccountNumber = AccountNumber Integer
data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
           = putStrLn $ name ++ " " ++ show acctNum
