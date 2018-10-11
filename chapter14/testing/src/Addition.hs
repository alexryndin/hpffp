module Addition where

import Test.Hspec
import Test.QuickCheck

-- main :: IO ()
-- main = hspec $ do
--   describe "Addition" $ do
--     it "1 + 1 is greater than 1" $ do
--       (1 + 1) > 1 `shouldBe` True
--     it "2 + 2 is equal to 4" $ do
--       2 + 2 `shouldBe` 4

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > (1 :: Integer) `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      ((2 + 2) :: Integer) `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Rmult" $ do
    it "15 * 3 is 45" $ do
      rmult 15 3 `shouldBe` (45 :: Integer)
    it "22 * 0 is 0" $ do
      rmult 22 0 `shouldBe` (0 :: Integer)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x
runQc :: IO ()
runQc = quickCheck prop_additionGreater

sayHello :: IO ()
sayHello = putStrLn "hello!"


rmult :: (Integral a) => a -> a -> a
rmult 0 _ = 0
rmult _ 0 = 0
rmult a b | a < 0 && b < 0 =   rmult (-a) (-b)
          | a < 0          = - rmult (-a)   b
          | b < 0          = - rmult   a  (-b)
          | otherwise = let
              go 0 _ = 0
              go a' b' = b' + go (a'-1) b'
              in go a b

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
-- What QuickCheck actually does
-- so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
