-- A few observations that reduce the search space:
-- For n=0, b must be a prime (positive).
-- For n=1, a must be odd.
-- For n=1, 1 + a + b >= 1 (i.e. a >= -b)

import Data.Numbers.Primes
import Data.List
import Data.Ord

--as = [-999..999]
bs = takeWhile (<= 1000) primes

cs = [(a,b) | b <- bs,
              a <- [-b, -b+2 .. 999]]

f a b n = n^2 + a*n + b

primeLists = map (\(a,b) -> ((a, b), takeWhile isPrime $ map (f a b) [0..])) cs

best = maximumBy (comparing (length . snd)) primeLists

answer = ((fst $ fst best) * (snd $ fst best), length $ snd best)

main = print answer