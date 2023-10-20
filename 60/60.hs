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

{- Returns all subsequences of size k from a list.   
   Is equivalent to (\k -> filter ((== k) . length) . subsequences)
   e.g. kSubsequences 3 "abcde" == ["abc","abd","acd","bcd","abe","ace","bce","ade","bde","cde"]
   Note: subsequences are really just ordered subsets. Thus for an unordered input list
   without repetition, it is equivalently returning subsets.
   -}
kSubsequences :: Integral a => a -> [b] -> [[b]]
kSubsequences _ [] = []
kSubsequences 1 list = map singleton list
kSubsequences k (x:xs) = (map (x:) (kSubsequences (k-1) xs))
                    ++ (kSubsequences k xs)

-- needed to implement a specific k=2 in order to use output in isSetPairwisePrime
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = (map (x,) xs) ++ (pairs xs)

isPairPrime :: Integral a => a -> a -> Bool
isPairPrime p q = (isPrime $ unDigits 10 (dp++dq)) && (isPrime $ unDigits 10 (dq++dp))
                    where [dp, dq] = map (digits 10) [p,q]

isSetPairwisePrime :: Integral a => [a] -> Bool
isSetPairwisePrime = (all (uncurry isPairPrime)) . pairs

-- list comprehensions allowing clearer code
candidateSets :: Integral a => Int -> [(a,[a])]
candidateSets groupSize = [(p,set) | p <- (drop 2 primes),
                           qs <- [filter (isPairPrime p) $ 3:(takeWhile (< p) $ drop 3 primes)], -- reduce search space, start with pair primes
                           --(length qs) >= (groupSize - 1), -- remove sets that are too small (for efficiency, unnecessary)
                           set <- (kSubsequences (groupSize - 1) qs), -- generate all possible subsets of the right size
                           isSetPairwisePrime set]

target = head $ candidateSets 5

answer = (target, (fst target) + (sum $ snd target))

main = print answer