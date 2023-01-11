{-
Resilient fractions a/d are those where a is relatively prime to d.
Euler's totient function t(d) gives us the number of integers relatively prime to d.
We want to find the lowest d for which R(d) is below 15499/94744, thus we need to
minimize t(d)/(d-1).
-}

import Data.Numbers.Primes
import Data.List

-- Primorial numbers, product of first n primes, OEIS #A002110
primorials = tail $ scanl (*) 1 primes

target = 15499 `intdiv` 94744

upperBound = 6469693230

--intdiv :: (Integral a, Fractional b) => a -> a -> b
intdiv a b = (fromIntegral a) / (fromIntegral b)

isFactorOf :: Integral a => a -> a -> Bool
isFactorOf x n = n `mod` x == 0

isCoprime :: Integral a => a -> a -> Bool
isCoprime n d = gcd n d == 1

-- [2,2,2,2,5,5,5] -> [(2,4),(5,3)]
groupCount :: Eq a => [a] -> [(a, Int)]
groupCount = map (\(x:xs) -> (x,1+(length xs))) . group

primeFactorsCount :: Integral a => a -> [(a, Int)]
primeFactorsCount = groupCount . primeFactors

composeFactors :: Integral a => [(a,a)] -> a
composeFactors = foldl (\a (p,k) -> a * p^k) 1

resilientFracs :: Integral a => a -> [a]
resilientFracs d = 1 : filter (isCoprime d) [2..d-1]

resilience :: (Integral a, Fractional b) => a -> b
resilience d = (length $ resilientFracs d) `intdiv` (d-1)

-- returns a list of lists of the first elements of a list
heads :: [a] -> [[a]]
heads = tail . (scanl (\l e -> l ++ [e]) [])
-- equivalent to Data.List.inits

--firstByMaxResilience :: (Ord a, Fractional a, Integral b) => a -> b
firstByMaxResilience r = head $ filter (\d -> (resilience d) < r) primorials

debug = zip primorials (map resilience primorials)

-- i is the 0-based index of prime and pm is the primorial number #i
fastResilientFracs i pm_i = 1 : (dropWhile (< primes !! i) $ takeWhile (< pm_i) primes)

fastResilience i pm_i = (length $ fastResilientFracs i pm_i) `intdiv` (pm_i-1)

totient :: Integral a => [(a,a)] -> a
totient = foldl (\a (p,k) -> a * p^(k-1) * (p-1)) 1

totient1 :: Integral a => [a] -> a
totient1 = (foldl (*) 1) . (map (subtract 1))

fastestResilience :: (Integral a, Fractional b) => [(a,a)] -> b
fastestResilience ps = (totient ps) `intdiv` ((composeFactors ps) - 1)

fastestResilience1 :: (Integral a, Fractional b) => [a] -> b
fastestResilience1 ps = (totient1 ps) `intdiv` (product ps - 1)

fastestFirstByMaxResilience r = head $ filter (\c -> (fastestResilience . primeFactorsCount $ c) < r) [2,4..upperBound]

primorialSub = tail $ subsequences primes

evenFaster r = take 20 $ filter (\c -> (fastestResilience1 c) < r) primorialSub

answer = fastestFirstByMaxResilience target

main = print answer

--zip3