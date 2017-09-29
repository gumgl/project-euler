import Data.List
import Data.Digits

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

isList9Pandigital d = (sort d) == [1..9]

isTarget n = (isList9Pandigital $ digits 10 (n `mod` 1000000000)) && (isList9Pandigital $ take 9 $ digits 10 n)
 
answer = snd $ head $ dropWhile (\(fn,n) -> (not . isTarget) fn) (zip fibs [1..])