module Euler.P49.Solution where

import Data.List
import Data.Numbers.Primes

-- Nicely filter and reformat as tuple (to pass as arguments) in one line:
pairwiseCombos l = [(x,y) | [x,y] <- subsequences l]

combinations size = filter ((== size) . length) . subsequences

triples = combinations 3

isArithmeticSequence [x,y,z] = x /= y && (y-x) == (z-y)
isArithmeticSequence _ = False

isCandidate n = 1000 <= n && n <= 9999

solutions = 
  map (concatMap show) $
  concatMap (
    (filter isArithmeticSequence) .
    triples .
    sort .
    (filter isPrime) .
    (filter isCandidate) .
    (map read) . 
    permutations . 
    show
  )
  (filter isCandidate primes)

answer = take (3*2) solutions

main = print answer