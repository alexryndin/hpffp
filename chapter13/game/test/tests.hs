import Test.Hspec
import Test.QuickCheck
import qualified Main as M

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "fills 'c' character in word" $ do
      fillInCharacter (M.Puzzle "myword" [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] "" 0) 'w' `shouldBe` (M.Puzzle "myword" [Nothing, Nothing, Just 'w', Nothing, Nothing, Nothing] "w" 0)
