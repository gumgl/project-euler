import Data.List
import Data.Digits
import Data.Ratio

-- to filter trivial cases
isTrivial a b = (a `mod` 10 == 0) && (b `mod` 10 == 0)

-- if removing the same digits from two 2-digit numbers 
canCancel a b = 
    let da = (digits 10 a)
        db = (digits 10 b)
        common = intersect da db
        na = unDigits 10 (da\\common)
        nb = unDigits 10 (db\\common)
    in     common /= [0]
        && (length common == 1)
        && (a%b) == (na%nb)

pool = [10..99]

--candidates :: Integral a => [a] -> [a] -> [(a,a)]
candidates [] = []
candidates (a:as) = (map (\i -> (a,i)) as) ++ (candidates as)

filter (uncurry canCancel) (candidates pool)



--fractions = (x:xs) 