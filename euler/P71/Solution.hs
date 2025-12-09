module Euler.P71.Solution where

import Data.List ( minimumBy )
import Data.Function ( on )
import Data.Ratio ( (%), numerator, denominator, Ratio )

-- Select one fraction for every d where n/d is as close to target but below it
fractions :: Ratio Integer -> Integer-> [Ratio Integer]
fractions target maxD =
  [n % d | d <- [2..maxD],
   let n = ((numerator target) * d) `div` (denominator target), -- int div rounding down
   n % d < target] -- filter fractions that reduce to target

-- Select the one with the smallest difference from target
answer :: Ratio Integer -> Integer -> Ratio Integer
answer target = minimumBy (compare `on` (target -)) . (fractions target)

main = print $ answer (3 % 7) (10^6) 