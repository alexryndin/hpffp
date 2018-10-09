import Data.Char
import Data.List
import Control.Monad (forever)
import System.Exit (exitSuccess)

vigenere :: String -> String -> String
vigenere w msg = let
  h n' c = if (c>'A')
           then chr ((charn c + n') `mod` 26 + shift c)
           else c
  in zipWith h keyl msg
  where shift c = case isUpper c of
          True  -> ord 'A'
          False -> ord 'a'
        charn c = ord c - shift c
        --n' = n `mod` 26
        key = map ((flip (-)) (ord 'A') . (ord . toUpper)) w
        keyl = take (length msg) (cycle key)

unvigenere :: String -> String -> String
unvigenere w msg = let
  h n' c = if (c>'A')
           then chr ((charn c + (26-n')) `mod` 26 + shift c)
           else c
  in zipWith h keyl msg
  where shift c = case isUpper c of
          True  -> ord 'A'
          False -> ord 'a'
        charn c = ord c - shift c
        --n' = n `mod` 26
        key = map ((flip (-)) (ord 'A') . (ord . toUpper)) w
        keyl = take (length msg) (cycle key)
--uncaesar n = caesar (26 - n `mod` 26)

decrypt :: IO ()
decrypt = do
  putStrLn $ "Phrase to decrypt?"
  str <- getLine
  putStrLn $ "Key?"
  key <- getLine
  case key of
    []  ->  putStrLn "Your key must contain at least one symbol" >> encrypt
    _   -> do putStrLn $ "Your decrypted phrase is : " ++ unvigenere key str
              exitSuccess

encrypt :: IO ()
encrypt = do
  putStrLn $ "Phrase to encrypt?"
  str <- getLine
  putStrLn $ "Key?"
  key <- getLine
  case key of
    []  ->  putStrLn "Your key must contain at least one symbol" >> encrypt
    _   -> do putStrLn $ "Your encrypted phrase is : " ++ vigenere key str
              exitSuccess

main :: IO ()
main = forever $ do
  putStrLn $ "You want to? \n 1. Encrypt \n 2. Decrypt"
  select <- getLine
  case select of
    ['1'] -> do encrypt
    ['2'] -> do decrypt
    _     -> putStrLn "Please, select only 1 or 2"

