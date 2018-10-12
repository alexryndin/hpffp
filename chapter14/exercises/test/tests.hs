module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Exercises as E
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      E.digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      E.digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      E.digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      E.digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      E.wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      E.wordNumber 9001
        `shouldBe` "nine-zero-zero-one"
  describe "half" $ do
    it "2 * half x == x" $ do
      property $ \x -> x == ((*2) . E.half) x
  describe "listOrdered" $ do
    it "check if order function works properly with int list" $ do
      property $ (E.listOrdered . sort :: [Int] -> Bool)
    it "check if order function works properly with string list" $ do
      property $ (E.listOrdered . sort :: String -> Bool)
  describe "check commutative and assotiative functions" $ do
    it "plusAssociative" $ do
     property $ (E.plusAssociative :: Int -> Int -> Int -> Bool)
    it "plusCommutative" $ do
      property $ (E.plusCommutative :: Int -> Int -> Bool)
    it "multAssociative" $ do
      property $ (E.multAssociative :: Int -> Int -> Int -> Bool)
    it "multCommutative" $ do
      property $(E.multCommutative :: Int -> Int -> Bool)
  describe "check quot and rem laws" $ do
    it "(quot x y)*y + (rem x y) == x" $ do
      property $ (E.quotLaw :: Integer -> Integer -> Bool)
    it "(div x y)*y + (mod x y) == x" $ do
      property $ (E.divLaw :: Integer -> Integer -> Bool)
  describe "Check pow (^) commuttativity and assotiativity (should fail)" $ do
    it "(x ^ (y ^ z) == (x ^ y) ^ z" $ do
      property $ (E.powAssociative :: Integer -> Integer -> Integer -> Bool)
    it "x ^ y == y ^ x" $ do
      property $ (E.powCommutative :: Integer -> Integer -> Bool)
  describe "Check list reverse" $ do
    it "reverse reverse x == x" $ do
      property $ (E.listReverse :: [Integer] -> Bool)
  describe "Check dollar" $ do
    it "((-) x $ (x+x)) == (-) x (x+x)" $ do
      property $ (E.propDollar :: Integer -> Bool)
  describe "Check prop and concat functions" $ do
    it "foldr (:) == (++)" $ do
      property $ (E.propCons :: [Integer] -> [Integer] -> Bool)
    it "foldr (++) [] == concat" $ do
      property $ (E.propConcat :: [[Integer]] -> Bool)
  describe "Check length func" $ do
    it "length (take n xs) == n" $ do
      property $ (E.propex10 :: Int -> [Integer] -> Bool)
  describe "Check read func" $ do
    it "(read (show x)) == x" $ do
      property $ (E.propRead :: [Integer] -> Bool)
  describe "Check square Identity" $ do
    it "(square . sqrt) x == x" $ do
      property $ (E.squareIdentity :: Double -> Bool)

runQc :: IO ()
runQc = do
  quickCheck E.f1
  quickCheck E.f2
