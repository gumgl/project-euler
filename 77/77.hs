{- cabal:
build-depends: base, primes
-}

import Data.Numbers.Primes
import Data.List (find)

p 0 _ = 1 -- Total fully reached, valid partition
p 1 _ = 0 -- Non-prime remainder, invalid partition
p _ [] = 0 -- Remaining total but no more primes to use
p total (n:ns)
    | n > total = p total ns -- We have used up all of n, move onto ns
    | otherwise = (p (total - n) (n:ns)) -- Use one n of potentially many
                + (p total ns) -- Do not use n, total remains unchanged

-- Prime P(N)) partition count
pp n = p n (reverse $ takeWhile (<n) primes)

target = find (\(_, pn) -> pn > 5000) $ map (\n -> (n, pp n)) [1..]

main = print target