import Control.Applicative

data Person = Person Name Age deriving Show
type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
                True -> Right age
                False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
                  True -> Right name
                  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> Either [PersonInvalid] Person

mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
