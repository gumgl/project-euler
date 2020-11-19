-- There might be a clever way to work out the digits on paper
-- by counting the number sizes, but this is faster.

import Data.Char (ord)

digitToInt d = (ord d) - (ord '0')

frac = concat $ map show [0..]

digits = map (\i -> digitToInt $ frac !! (10^i)) [0..6]

answer = product digits

main = print answer