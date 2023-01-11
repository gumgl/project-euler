import Data.List
import Data.Numbers.Primes
import Data.Digits

{- Using base 10 digit shuffling of prime numbers, I figured there is no faster way to
   identify such groups but to brute-force-check through all possible prime sets.
   This solution thus focuses on reducing the search space.
   First we start by producing a list of primes and their smaller pair primes.
   Afterwards we generate all possible 4-sets for each prime.
   We finish by looking for sets containing only prime pairs, then return the first match.
-}

-- returns all subsequences of size k from a list
kSubsets :: Integral a => a -> [b] -> [[b]]
kSubsets _ [] = []
kSubsets 1 list = map singleton list
kSubsets k (x:xs) = (map (x:) (kSubsets (k-1) xs))
                    ++ (kSubsets k xs)

-- needed to implement a specific k=2 in order to use output in pairwise function
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = (map (x,) xs) ++ (pairs xs)

isPairPrime :: Integral a => a -> a -> Bool
isPairPrime p q = (isPrime $ unDigits 10 (dp++dq)) && (isPrime $ unDigits 10 (dq++dp))
                    where [dp, dq] = map (digits 10) [p,q]

isSetPairwisePrime :: Integral a => [a] -> Bool
isSetPairwisePrime = (all (uncurry isPairPrime)) . pairs

-- list comprehensions allowing clear code
candidateSets :: Integral a => Int -> [(a,[a])]
candidateSets groupSize = [(p,set) | p <- (drop 2 primes),
                           qs <- [filter (isPairPrime p) $ 3:(takeWhile (< p) $ drop 3 primes)], -- reduce search space, start with pair primes
                           --(length qs) >= (groupSize - 1), -- remove sets that are too small (for efficiency, unnecessary)
                           set <- (kSubsets (groupSize - 1) qs), -- generate all possible subsets of the right size
                           isSetPairwisePrime set]

target = head $ candidateSets 5

answer = (target, (fst target) + (sum $ snd target))

main = print answer