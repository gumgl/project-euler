pentagonals = map (\n -> floor (n*(3*n-1)/2)) [1..]

--isPentagonal :: Integer -> Bool
isPentagonal n = (snd (properFraction ((sqrt(24 * fromIntegral n + 1) + 1)/6))) == 0

--isCandidate :: Integer -> Integer -> Bool
isCandidate j k = isPentagonal(j+k) && isPentagonal(abs(j-k))

candidates = concat $ 
             map (\i -> 
               map (\j -> (i,j)) $
               filter (isCandidate i) (
                 takeWhile (<i) pentagonals))
               pentagonals

candidate = head candidates

answer = fst candidate - snd candidate

main = print answer
