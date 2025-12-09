module Euler.P44.Solution where

pentagonals = map (\n -> floor (n*(3*n-1)/2)) [1..]

--isPentagonal :: Integer -> Bool
isPentagonal n = (snd (properFraction ((sqrt(24 * fromIntegral n + 1) + 1)/6))) == 0

--isCandidate :: Integer -> Integer -> Bool
isCandidate j k = isPentagonal(j+k) && isPentagonal(abs(j-k))

candidates = concatMap (\i -> 
               map (\j -> (i,j)) $
               filter (isCandidate i) (
                 takeWhile (<i) pentagonals))
               pentagonals

answer = let (j,k)=(head candidates) in j-k

main = print answer
