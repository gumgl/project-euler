module Common where

import Data.List

-- Count occurrences of each element in a list
count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

countIf :: (a -> Bool) -> [a] -> Int
countIf predicate = length . filter predicate

-- Apply a function to each pair of consecutive elements in a list
consecutiveMap :: (a -> a -> b) -> [a] -> [b]
consecutiveMap f [] = []
consecutiveMap f ls = zipWith f (tail ls) ls

absoluteDifference :: Num a => a -> a -> a
absoluteDifference a b = abs (a - b)

-- Source - https://stackoverflow.com/a/59007616
-- Retrieved 2025-11-13, License - CC BY-SA 4.0
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

-- Source - https://stackoverflow.com/a/52605612
-- Posted by dopamane, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-11-13, License - CC BY-SA 4.0

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns
