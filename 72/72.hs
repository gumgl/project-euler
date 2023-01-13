import Data.List ( group )
import Data.Numbers.Primes ( primeFactors )
import Data.Function ( (&))

groupCount :: Eq a => [a] -> [(a, Int)]
groupCount = map (\(x:xs) -> (x,1+(length xs))) . group

primeFactorsCount :: Integral a => a -> [(a, Int)]
primeFactorsCount = groupCount . primeFactors

totient :: Integral a => [(a,a)] -> a
totient = foldl (\a (p,k) -> a * p^(k-1) * (p-1)) 1

answer maxD = [2..maxD]
  & map (totient . primeFactorsCount)
  & sum

main = print $ answer $ 10^6