module Euler.P57.Solution where

import Data.Digits

numLength = length . (digits 10)

-- Will contract a fraction of the form m + 1/(a/b)
contract :: (Integral a) => a -> (a,a) -> (a,a)
contract m (a,b) = (m*a + b, a)

-- Computes the continued fraction for sqrt of 2 with n iterations
sqrt2 :: (Integral a, Eq a) => a -> (a,a)
sqrt2 n = contract 1 (iterate n)
  where iterate 1 = (2,1)
        iterate i = contract 2 (iterate $ i-1)

answer = length $ filter (\(a,b) -> (numLength a) > (numLength b)) $ map sqrt2 [1..1000]