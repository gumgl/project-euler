import Data.Digits
import Data.List
import Data.Numbers.Primes

isTruncatable n = all isPrime $ versions n

versions n = 
    let digs = digits 10 n
    in sort $ map (unDigits 10) (digs : (triml digs) ++ (trimr digs))



n = 3797

triml [x] = []
triml (x:xs) = xs : triml xs
trimr xs = map reverse $ triml $ reverse xs

found = take 11 $ filter isTruncatable (filter (>9) primes)

answer = sum found