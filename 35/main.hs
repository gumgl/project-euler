import Data.List
import Data.Digits
import Data.Numbers.Primes
import qualified Data.Map as Map
import qualified Data.Set as Set

below _ [] = []
below max (x:xs) = if (x < max) then [x] ++ (below max xs) else (below max xs)

-- get content of list below max and stop (when list is growing)
belowStop :: Integral a => a -> [a] -> [a]
belowStop _ [] = []
belowStop max (x:xs) = if (x < max) then [x] ++ (belowStop max xs) else []

maxim = 1000000

theprimes = Set.fromList (belowStop maxim primes)
-- TODO: optimize so that we don't look at numbers containing an even digit with filter&map

-- rotate a list
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- rotations of a list
rotations :: [a] -> [[a]]
rotations list = map (\i -> rotate i list) [0 .. length(list)-1]

-- rotations of an integer
nrotations x = map (\i -> unDigits 10 i) (rotations (digits 10 x))

-- generate all rotations, then each back to numbers, then take the minimum
hash x = minimum (nrotations x)

-- If there is a rotation smaller than x not in the set, it should've been added before and is therefore not a circular prime
--addNumber :: Integer a => a -> Set.Set a -> Set.Set a
--addNumber [] s = s
--addNumber (x:xs) s = if (null (filter (\i -> (not (Set.member i s))) (below x (nrotations x)))) then (addNumber xs (Set.insert x s)) else (addNumber xs s)

isPrimeEfficient x = Set.member x theprimes

-- generate all circular primes in a set
isCircular x = all isPrimeEfficient (nrotations x)

circulars = filter isCircular [2..maxim]
