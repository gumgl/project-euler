module Euler.P39.Solution where

import Data.List
import Data.Ord
import qualified Data.Map as Map

maxP = 1000

perimeters = [p
  | a <- [1..maxP], 
    b <- [1..a],
    let cFloat = sqrt $ fromInteger (a^2 + b^2),
    let cInt = floor cFloat,
    let p = a + b + cInt,
    cFloat == fromInteger cInt,
    p <= maxP]

counts = Map.fromListWith
  (+)
  (map (\p -> (p,1)) perimeters)

answer = maximumBy (comparing (counts Map.!)) perimeters

main = print (answer, counts Map.! answer)