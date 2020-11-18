import Data.Digits
import Data.List
import Data.Maybe

-- When we search numbers of i digits, if the maximum sum that you can
-- generate (i*9^5) doesn't even have i digits [>10^(i-1)], that is out of bounds
upperBound = (fromMaybe 7 $ find (\i -> 9^5 * i < 10^(i-1)) [1..]) -1

isTarget n = ((sum $ map (^5) $ digits 10 n) == n)

searchSpace = [2..10^upperBound]

targets = filter isTarget searchSpace

answer = sum targets

main = print answer