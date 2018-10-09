import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case checkPalindrome line1 of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!" >> exitSuccess

checkPalindrome :: String -> Bool
checkPalindrome str = s == reverse s
  where s = prepareStr str

prepareStr :: String -> String
prepareStr = map toLower . filter (not . flip elem "?';:$#@! ")
