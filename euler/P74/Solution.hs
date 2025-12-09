module Euler.P74.Solution where

import Data.Digits (digits)
import qualified Data.IntSet as IntSet
import Data.Function ( (&) )

fac x = case x of
  0 -> 1
  1 -> 1
  2 -> 2
  3 -> 6
  4 -> 24
  5 -> 120
  6 -> 720
  7 -> 5040
  8 -> 40320
  9 -> 362880
  n -> n * (fac (n-1))

next :: Integral b => b -> b
next n = digits 10 n & map fac & sum

countNonRepeatingTerms :: Int -> Int
countNonRepeatingTerms = traverse IntSet.empty
  where
    traverse seen x =
      if IntSet.member x seen then 0
      else 1 + (traverse (IntSet.insert x seen) (next x))

answer = [1..10^6-1]
  & filter ((==60) . countNonRepeatingTerms)
  & length

main = print answer