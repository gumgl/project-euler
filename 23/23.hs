import Data.List
import Data.List.Extra
import qualified Math.NumberTheory.Primes.Factorisation as F

-- because they include n in the list whereas PE does not
divisorSum n = (F.divisorSum n) - n

isPerfect n = (divisorSum n) == n
isAbundant n = (divisorSum n) > n

abundants = filter isAbundant [1..28123]

sumsOfAbundants = nubOrd [ (a+b) | a<-abundants, b<-abundants, (a+b)<=28123 ]

target = [1..28123] \\ sumsOfAbundants

answer = sum target

-- integers which cannot be written as the sum of two abundant numbers
--target = filter isTarget [24..28123]
