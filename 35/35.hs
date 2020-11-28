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

-- rotate a list
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- rotations of a list
rotations :: [a] -> [[a]]
rotations list = map (\i -> rotate i list) [0 .. length(list)-1]

isEven x = (x `mod` 2) == 0
isOdd x = not $ isEven x

allOdd x = all isOdd (digits 10 x)

primesList = belowStop 1000000 (2:(filter allOdd primes))
primesSet = Set.fromList primesList

-- rotations of an integer
nrotations x = map (\i -> unDigits 10 i) (rotations (digits 10 x))

isPrimeEfficient x = Set.member x primesSet

-- generate all circular primes in a set
isCircular x = all isPrimeEfficient (nrotations x)

circulars = filter isCircular primesList

answer = length circulars

main = print answer
