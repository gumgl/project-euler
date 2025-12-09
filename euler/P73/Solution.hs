module Euler.P73.Solution where

import Data.Ratio ( (%), numerator, denominator, Ratio )
import qualified Data.Set as Set

-- For every d, select all fractions between bounds
fractions :: Integral a => Ratio a -> Ratio a -> a -> [Ratio a]
fractions lower upper maxD =
  [ration | d <- [2..maxD],
    let lowerN = ((numerator lower) * d) `div` (denominator lower),
    let upperN = ((numerator upper) * d) `div` (denominator upper) + 1,
    n <- [lowerN .. upperN],
    let ration = n % d,
    (lower < ration) && (ration < upper)] -- filter fractions that reduce to bounds

-- Insert list into set for a distinct count: O(n log n)
answer :: Integral a => Ratio a -> Ratio a -> a -> Int
answer l u = Set.size . Set.fromList . (fractions l u)

main = print $ answer (1 % 3) (1 % 2) 12000