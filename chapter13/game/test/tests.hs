module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Game as M

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "fills 'c' character in word" $ do
      (M.fillInCharacter (M.Puzzle "myword" [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] "" 0) 'w') `shouldBe` (M.Puzzle "myword" [Nothing, Nothing, Just 'w', Nothing, Nothing, Nothing] "w" 0)
