module Euler.P14.Solution where

-- Really inefficient, takes too long to run. Try compiling it.

import Data.List
import Data.Ord

-- The sequence in the Collatz conjecture
--collatz :: Int -> [Int]
collatz 1 = [1]
collatz n =
  if n `mod` 2 == 0 then n:(collatz (n `div` 2))
  else n:(collatz (3*n+1))

seqLengths = map (\i -> (i,length $ collatz i)) [1..999999]

answer = maximumBy (comparing snd) seqLengths

main = print (fst answer)
