newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show
-- FarmerType is a Sum
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show
data FarmerRec = FarmerRec { name :: Name
                           , acres :: Acres
                           , farmerType :: FarmerType } deriving Show
isDairyFarmer :: FarmerRec -> Bool
isDairyFarmer (FarmerRec _ _ DairyFarmer) = True
isDairyFarmer _ = False
