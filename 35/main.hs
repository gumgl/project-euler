import Data.List
import Data.Digits
import Data.Numbers.Primes
import qualified Data.Map as Map 
maxim = 999999

-- get content of list below max
below :: Integral a => a -> [a] -> [a]
below _ [] = []
below max (x:xs) = if (x <= max) then [x] ++ below(max xs) else []

-- rotate a list
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- rotations of a list
rotations :: [a] -> [[a]]
rotations list = map (\i -> rotate i list) [0 .. length(list)-1]

nrotations x = map (\i -> unDigits 10 i) (rotations (digits 10 x))

-- generate all rotations, then each back to numbers, then take the minimum
hash x = minimum (nrotations x)

--map (\i -> i) (
