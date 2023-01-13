import Data.List
import Data.Function ( on )
import Data.Ratio ( (%), numerator, denominator, Ratio )

target = 3 % 7

-- Select one fraction for every d where n/d is as close to target but below
fractions :: Integer -> [Ratio Integer]
fractions x = [n % d | d <- [2..x],
               let n = ((numerator target) * d) `div` (denominator target), -- int div rounding down
               n % d < target] -- filter fractions that reduce to target

-- Select the one with the smallest difference from target
answer :: Integer -> Ratio Integer
answer = minimumBy (compare `on` (target -)) . fractions

main = print $ answer $ 10^6