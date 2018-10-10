module Addition where

import Test.Hspec

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
  describe "Rmult" $ do
    it "15 * 3 is 45" $ do
      rmult 15 3 `shouldBe` (45 :: Integer)
    it "22 * 0 is 0" $ do
      rmult 22 0 `shouldBe` (0 :: Integer)

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
