module Euler.P59.Solution where

import Data.List
import Data.List.Split ( splitOn )
import Data.Char ( chr, ord )
import Data.Word ( Word8 )
import Data.Bits
import qualified Data.Ord

encryptXOR :: (Bits a) => [a] -> [a] -> [a]
encryptXOR message key = zipWith xor message (cycle key)

showWord8 :: [Word8] -> String
showWord8 = map (chr . fromIntegral)

countWord8 :: [Word8] -> Int
countWord8 = sum . map fromIntegral

-- https://stackoverflow.com/a/48131849/646562 & https://en.wikipedia.org/wiki/Most_common_words_in_English
countCommonWords :: String -> Int
countCommonWords = length . intersect commonWords . words
  where commonWords = ["the", "be", "to", "of", "and", "a", "in", "that", "have", "I", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at"]

keys :: [[Word8]]
keys = [[c1,c2,c3] | c1 <- lowercaseValues,
                    c2 <- lowercaseValues,
                    c3 <- lowercaseValues]
  where lowercaseValues = map fromIntegral [(ord 'a')..(ord 'z')] :: [Word8]

main :: IO ()
main = do
  contents <- readFile "cipher.txt"
  let ciphertext = map (fromInteger . read) $ splitOn "," contents
      keysWithCommonWordCounts = sortOn (Data.Ord.Down . snd) $ map (\k -> (k, countCommonWords $ showWord8 $ encryptXOR ciphertext k)) keys
      plaintext = encryptXOR ciphertext (fst $ head keysWithCommonWordCounts)
  print $ countWord8 plaintext
  putStrLn $ showWord8 plaintext