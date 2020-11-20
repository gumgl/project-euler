import Data.List
import Data.Numbers.Primes

{- Too slow:

primes' = search [2..] where search (p:xs) = p : search [x | x <- xs, x `mod` p /= 0]

primeFactors' n = 
  case smallestPrimeFactor of
    Just p -> p : primeFactors' (n `div` p)
    Nothing -> []
  where smallestPrimeFactor = find (\p -> (n `rem` p) == 0) (takeWhile (<n) primes')
-}

{- Combines duplicates of a sorted list using a given function
Examples:
combineDups (*) [2,2,2,3,3,13,59] = [8,9,13,59]
combineDups (curry fst) "aaabbccc" = "abc"

First implementation:

combineDups _ [] = []
combineDups f (x:xs) =
  (foldr1 f dups) : (combineDups f rest)
  where (dups, rest) = (span (== x) (x:xs))

Unnecessary function in the end:
pairwiseCombos l = [(x,y) | [x,y] <- subsequences l]
-}

--combineDups :: Eq a => (a -> a -> a) -> [a] -> [a]
combineDups f = map (foldr1 f) . group

solutions n =
    filter (\i ->
      let precedingFactors = map getFactors [i-n+1..i]
      in all ((==n) . length) precedingFactors
        && (hasDuplicates . concat) precedingFactors
    )
    [n..]
  where
    getFactors = (combineDups (*)) . primeFactors
    hasDuplicates = any ((>= 1) . length) . group . sort

solution n = let target = head $ solutions n
  in [target-n+1..target]

main = print $ solution 4