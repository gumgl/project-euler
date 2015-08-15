import Data.List(permutations)
import Data.Digits
import Data.Numbers.Primes

pandigitals = permutations [0..9]

--returns whether the number formed by the first 3 digits is divisible by x
isSSD :: [Int] -> [Int] -> Bool
isSSD (y1:y2:y3:ys) (x:xs) = if ((unDigits 10 [y1,y2,y3]) `mod` x == 0)
    then isSSD (y2:y3:ys) xs
    else False
isSSD _ _ = True

pandigitalSSDs = filter (\a -> isSSD a (1:primes)) pandigitals

answer = sum (map (unDigits 10) pandigitalSSDs)

