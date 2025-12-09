module Euler.P98.Solution where

-- INCOMPLETE

import Data.List
import Data.List.Split
--import Data.Map as Map
import Data.Function ( (&), on )

import Math.NumberTheory.Roots

import qualified Data.Map as M
-- Remove the first and last element of a list
trim :: [a] -> [a]
trim = tail . init

-- from https://stackoverflow.com/a/53403093/646562
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f =
  let unpack = fmap snd . M.toList
      fld m a = case M.lookup (f a) m of
        Nothing -> M.insert (f a) [a] m
        Just as -> M.insert (f a) (a:as) m
  in unpack . foldl fld M.empty

-- From a list of lists, keep pairs and for all lists longer, extract pairs
toPairs :: [[a]] -> [[a]]
toPairs b = (filter ((== 2) . length) b)
  ++ (b
      & filter ((> 2) . length)
      & concatMap (filter ((== 2) . length) . subsequences))

-- In base 10, a square number can end only with digits 0, 1, 4, 5, 6 or 9, as follows:

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

isAnagramSquarePair w1 w2 =
  isSquare 25

main :: IO ()
main = do
  contents <- readFile "words.txt"
  let wordList = filter ((>= 4) . length) $ map trim (splitOn "," contents)
      groupedWords = groupOn sort wordList
      anagrams = filter ((>= 2) . length) groupedWords
      pairs = sortBy (flip compare `on` (length . head)) $ toPairs anagrams
  mapM_ putStrLn (filter hasDuplicates (concat pairs))